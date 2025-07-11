# ------------------------------------
# 필요한 패키지
# ------------------------------------
tzl_pkgs <- c(
  "dplyr", "rstan", "posterior", "rstudioapi", "tibble",
  "tidyr", "knitr", "kableExtra", "stringr", "readr"
)
for (pkg in tzl_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
lapply(tzl_pkgs, library, character.only = TRUE)
set.seed(2025)

# ------------------------------------
# rstan 옵션
# ------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ------------------------------------
# 데이터 불러오기 및 전처리
# ------------------------------------
setwd("C:/Users/ahnhy/Desktop/석사논문/Replication")
df <- read.csv("data/cleaned_data/panel.csv", stringsAsFactors = FALSE)

# Year 칼럼이 “1975” 같은 숫자만 있을 때 모두 1975년이 되는 문제 방지
df$Year <- as.Date(paste0(df$Year, "-01-01"), format = "%Y-%m-%d")

df <- df %>%
  arrange(Country.ID, Year) %>%
  group_by(Country.ID) %>%
  mutate(
    d_young = r_young - r_imm,
    d_mid   = r_mid   - r_imm,
    d_old   = r_old   - r_imm
  ) %>%
  filter(!is.na(savingrate)) %>%
  ungroup()

# ------------------------------------
# 클러스터 정보 불러오기 및 기간 복원
# ------------------------------------
cluster_info <- read.csv("cluster_summary.csv", stringsAsFactors = FALSE)

cluster_periods <- cluster_info %>%
  select(cluster, elements) %>%
  # 기존: sep = ";\\s*" (세미콜론만)
  # 변경: sep = "[;,]\\s*" (세미콜론 또는 콤마)
  separate_rows(elements, sep = "[;,]\\s*") %>%
  filter(str_detect(elements, "^[0-9]{2}-[0-9]{2}[A-Z]{3}$")) %>%
  mutate(
    start2= str_extract(elements, "^[0-9]{2}"),
    end2= str_extract(elements, "(?<=-)[0-9]{2}(?=[A-Z]{3}$)"),
    Country.ID = str_extract(elements, "[A-Z]{3}$"),
    # 1) 두 자리 숫자를 1900/2000년대로 변환
    start = as.integer(start2) + if_else(as.integer(start2) >= 90, 1900, 2000), # 90 이상은 1900년대 (90-99), 그 외는 2000년대 (00-89)
    end= as.integer(end2)+ if_else(as.integer(end2)>= 90, 1900, 2000),
    # 2) 실제 데이터가 존재하는 1990~2024 사이로 클램핑
    start = pmax(start, 1990),
    end= pmin(end,2024)
  ) %>%
  select(cluster, Country.ID, start, end)

# ------------------------------------
# Stan 모델 및 공통 설정
# ------------------------------------
stan_code    <- paste(
  readLines('code/panel_varx_minn_iw_updated_fixedlambda.stan'),
  collapse = "\n"
)
target_model <- stan_model(model_code = stan_code, auto_write = TRUE)

M     <- ncol(select(df, gdp_p, cpi, interestrate, unem, savingrate, inv))
K     <- ncol(select(df, d_young, d_mid, d_old, popgrowth))  # 실제 변수명 확인!
p     <- 1
decay <- 1.0
df_iw <- M + 1
Psi_iw <- diag((0.02)^2, M)

# ------------------------------------
# 클러스터 단위 VARX 실행 함수
# ------------------------------------
run_varx_for_cluster <- function(cluster_id) {
  # (1) 이 클러스터에 속한 국가·기간 정보
  cps <- cluster_periods %>% filter(cluster == cluster_id)
  
  # (2) df와 결합 → 클러스터 풀링된 패널 생성
  df_cl <- df %>%
    inner_join(cps, by = "Country.ID", relationship = "many-to-many") %>%
    filter(
      Year >= as.Date(paste0(start, "-01-01")),
      Year <= as.Date(paste0(end,   "-12-31"))
    ) %>%
    arrange(Country.ID, Year)
  
  country_periods_for_cluster <- df_cl %>%
    distinct(Country.ID, start, end) %>%
    arrange(Country.ID, start)
  
  cat("\n===== Cluster", cluster_id, "국가별 할당 기간 =====\n")
  print(country_periods_for_cluster, n = Inf) 
  
  # (4) Y, X 행렬 생성
  y_mat <- as.matrix(select(df_cl, gdp_p, cpi, interestrate, unem, savingrate, inv))
  x_mat <- as.matrix(select(df_cl, d_young, d_mid, d_old, popgrowth))
  
  keep  <- complete.cases(y_mat, x_mat)
  y_mat <- y_mat[keep, ]; x_mat <- x_mat[keep, ]
  
  if (nrow(y_mat) < M + p) {
    message("Cluster ", cluster_id, ": 데이터 부족 → 스킵")
    return()
  }
  
  stan_data <- list(
    N       = nrow(y_mat),
    M       = M, K = K, p = p,
    Y       = y_mat, X = x_mat,
    decay   = decay, df_iw = df_iw, Psi_iw = Psi_iw,
    lambda1 = 0.437, lambda2 = 0.171, lambda3 = 0.177
  )
  
  # (5) Stan 샘플링
  fit <- sampling(
    object  = target_model,
    data    = stan_data,
    chains  = 4, iter = 4000, warmup = 2000,
    control = list(adapt_delta = 0.90)
  )
  
  # (6) 장기승수와 이민자 효과 계산
  A_arr <- rstan::extract(fit, "A")$A
  B_arr <- rstan::extract(fit, "B")$B
  S     <- dim(A_arr)[1]
  
  DLR_samples <- array(NA, dim = c(S, K, M))
  for (s in seq_len(S)) {
    A_s <- A_arr[s, 1, , ]; B_s <- B_arr[s, , ]
    DLR_samples[s, , ] <- t(solve(diag(M) - A_s) %*% t(B_s))
  }
  dlrs <- apply(DLR_samples, c(2,3), function(x) c(
    mean  = mean(x),
    lower = quantile(x, 0.05),
    upper = quantile(x, 0.95)
  ))
  
  imm_samps <- matrix(NA, nrow = S, ncol = M)
  for (s in seq_len(S)) {
    imm_samps[s, ] <- -colSums(DLR_samples[s, 1:3, ])
  }
  imm_ci <- t(apply(imm_samps, 2, function(x) c(
    mean  = mean(x),
    lower = quantile(x, 0.05),
    upper = quantile(x, 0.95)
  )))
  
  effects       <- c("young","mid","old","popgrowth","immigrant")
  colnames_list <- colnames(y_mat)
  
  mean_full  <- rbind(dlrs["mean",  , ], immigrant = imm_ci[, "mean"])
  lower_full <- rbind(dlrs["lower.5%", , ], immigrant = imm_ci[, "lower.5%"])
  upper_full <- rbind(dlrs["upper.95%", , ], immigrant = imm_ci[, "upper.95%"])
  
  rownames(mean_full)  <- effects; colnames(mean_full)  <- colnames_list
  rownames(lower_full) <- effects; colnames(lower_full) <- colnames_list
  rownames(upper_full) <- effects; colnames(upper_full) <- colnames_list
  
  results <- expand.grid(
    effect   = effects,
    variable = colnames_list,
    stringsAsFactors = FALSE
  )
  row_idx <- match(results$effect,   rownames(mean_full))
  col_idx <- match(results$variable, colnames_list)
  
  results$mean  <- mean_full[cbind(row_idx, col_idx)]
  results$lower <- lower_full[cbind(row_idx, col_idx)]
  results$upper <- upper_full[cbind(row_idx, col_idx)]
  
  # (7) LaTeX 테이블 생성
  results_cells <- results %>%
    mutate(cell = sprintf(
      "\\makecell[c]{%.3f \\\\ {\\scriptsize[%.3f, %.3f]}}",
      mean, lower, upper
    ))
  results_t <- results_cells %>%
    select(variable, effect, cell) %>%
    pivot_wider(
      id_cols     = variable,
      names_from  = effect,
      values_from = cell
    ) %>%
    arrange(variable)
  
  pop_vars <- colnames(results_t)[-1]
  col_spec <- paste0("l", paste(rep("c", length(pop_vars)), collapse = ""))
  
  lines <- c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{makecell}",
    "\\usepackage[margin=1in]{geometry}",
    "\\begin{document}",
    "\\begin{table}[!htbp]\\centering",
    sprintf("\\caption{Cluster %d: Effects of Population Variables}", cluster_id),
    "\\label{tab:effects}",
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule",
    paste0("Variable & ", paste(pop_vars, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  for (i in seq_len(nrow(results_t))) {
    var_name  <- results_t$variable[i]
    row_cells <- unname(results_t[i, -1])
    lines     <- c(
      lines,
      paste0(var_name, " & ", paste(row_cells, collapse = " & "), " \\\\")
    )
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}", "\\end{table}", "\\end{document}")
  
  out_dir <- file.path("출력", paste0("cluster_", cluster_id))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(
    results,
    file.path(out_dir, sprintf("cluster%02d_LReffect.csv", cluster_id)),
    row.names = FALSE
  )
  writeLines(
    lines,
    file.path(out_dir, sprintf("cluster%02d_LReffect.tex", cluster_id)),
    useBytes = TRUE
  )
  
  message("Cluster ", cluster_id, " 완료")
}

# ------------------------------------
# 모든 클러스터에 대해 실행
# ------------------------------------
for (cid in sort(unique(cluster_periods$cluster))) {
  try(run_varx_for_cluster(cid), silent = TRUE)
} 