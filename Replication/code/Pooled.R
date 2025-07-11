# panel_varx_minn_iw_updated_fixed.R
# VARX(p) with Minnesota prior, λ들에 Gamma 사전분포 적용
# 1) 설치 및 로드

setwd("C:/Users/ahnhy/Desktop/석사논문1")
tzl_pkgs <- c('dplyr','rstan','posterior','rstudioapi','tibble','tidyr','knitr','kableExtra','stringr')
for (pkg in tzl_pkgs) if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg)
lapply(tzl_pkgs, library, character.only=TRUE)

set.seed(2025)

# 2) rstan 옵션
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 3) 데이터 불러오기·전처리

df <- read.csv("data.csv", stringsAsFactors=FALSE)
df$Year <- as.Date(df$Year)
df <- df %>%
  arrange(Country.ID, Year) %>%
  group_by(Country.ID) %>%
  mutate(
    d_young = r_young - r_imm,
    d_mid   = r_mid   - r_imm,
    d_old   = r_old   - r_imm,
  ) %>%
  filter(!is.na(savingrate)) %>%
  ungroup()

# 4) 행렬 생성 및 NA 제거
library(dplyr)
y_mat <- as.matrix(select(df, gdp_p, cpi, interestrate, savingrate, unem, inv))
x_mat <- as.matrix(select(df, d_young, d_mid, d_old, pgr, logoil))
keep  <- complete.cases(y_mat, x_mat)
y_mat <- y_mat[keep,]; x_mat <- x_mat[keep,]
N <- nrow(y_mat); M <- ncol(y_mat); K <- ncol(x_mat); p <- 1

# 5) 하이퍼파라미터 설정
decay <- 1.0
df_iw <- M + 1
Psi_iw <- diag((0.02)^2, M)

# 6) Stan 모델 컴파일 및 샘플링
stan_code <- "data {
  int<lower=1> N;
  int<lower=1> M;
  int<lower=1> K;
  int<lower=1> p;
  matrix[N, M] Y;
  matrix[N, K] X;
  real<lower=0> decay;
  int<lower=M+1> df_iw;
  matrix[M, M] Psi_iw;
}
parameters {
  matrix[M, M] A[p];
  matrix[K, M] B;
  vector[M] alpha;
  real<lower=0> lambda1;
  real<lower=0> lambda2;
  real<lower=0> lambda3;
  cov_matrix[M] Sigma;
}
model {
  lambda1 ~ gamma(0.1, 0.1);
  lambda2 ~ gamma(0.1, 0.1);
  lambda3 ~ gamma(0.1, 0.1);
  for (lag in 1:p)
    for (i in 1:M)
      for (j in 1:M) {
        real sdA = lambda1 / pow(lag, decay) * (i==j ? 1 : lambda2);
        A[lag][i,j] ~ normal((i==j && lag==1) ? 1 : 0, sdA);
      }
  for (k in 1:K)
    for (i in 1:M) {
      B[k,i] ~ normal(0, lambda1 * lambda3);
    }
  alpha ~ normal(0, 1);
  Sigma ~ inv_wishart(df_iw, Psi_iw);
  for (t in (p+1):N) {
    vector[M] mu = alpha;
    for (lag in 1:p)
      mu += A[lag] * to_vector(Y[t-lag,]);
    mu += B' * to_vector(X[t,]);
    Y[t] ~ multi_normal(mu, Sigma);
  }
}"
writeLines(stan_code, 'panel_varx_minn_iw_updated_fixed.stan')

target_model <- stan_model(model_code=stan_code, auto_write=TRUE)
stan_data <- list(N=N,M=M,K=K,p=p,Y=y_mat,X=x_mat,decay=decay,df_iw=df_iw,Psi_iw=Psi_iw)
fit <- sampling(target_model,data=stan_data,chains=4,iter=2000,warmup=1000,control=list(adapt_delta=0.9))

# 7) 장기승수 및 95% CI 계산
A_arr <- rstan::extract(fit,'A')$A
B_arr <- rstan::extract(fit,'B')$B
S     <- dim(A_arr)[1]
DLR_samples <- array(NA, dim=c(S,K,M))
for (s in seq_len(S)) {
  A_s <- A_arr[s,1,,]
  B_s <- B_arr[s,,]
  DLR_samples[s,,] <- t(solve(diag(M) - A_s) %*% t(B_s))
}
dlrs <- apply(DLR_samples, c(2,3), function(x) c(
  mean  = mean(x),
  lower = quantile(x,0.025),
  upper = quantile(x,0.975)
))

# 8) 이민자 복원 효과 추출
imm_samps <- matrix(NA, nrow=S, ncol=M)
for (s in seq_len(S)) {
  imm_samps[s,] <- -colSums(DLR_samples[s,1:3, ])
}
imm_ci <- t(apply(imm_samps, 2, function(x) c(
  mean  = mean(x),
  lower = quantile(x,0.025),
  upper = quantile(x,0.975)
)))
 

effects <- c("young","mid","old","pgr","logoil","immigrant")

# 열 이름 지정 (필수!)
colnames_list <- colnames(y_mat)

# dlrs: dimensions [stat, K, M] — 1=mean,2=lower,3=upper
  mean_full  <- rbind(
    dlrs["mean", , ],
    immigrant = imm_ci[, "mean"]
  )
lower_full <- rbind(
  dlrs["lower.2.5%", , ],
  immigrant = imm_ci[, "lower.2.5%"]
)
upper_full <- rbind(
  dlrs["upper.97.5%", , ],
  immigrant = imm_ci[, "upper.97.5%"]
)

# 행과 열 이름을 정확히 설정
rownames(mean_full)  <- effects
colnames(mean_full)  <- colnames_list
rownames(lower_full) <- effects
colnames(lower_full) <- colnames_list
rownames(upper_full) <- effects
colnames(upper_full) <- colnames_list

# long format 생성
results <- expand.grid(
  effect   = effects,
  variable = colnames(y_mat),
  stringsAsFactors = FALSE
)

# 행, 열 인덱스 생성
row_idx <- match(results$effect, rownames(mean_full))
col_idx <- match(results$variable, colnames(mean_full))

# 값 추출
results$mean  <- mean_full[cbind(row_idx, col_idx)]
results$lower <- lower_full[cbind(row_idx, col_idx)]
results$upper <- upper_full[cbind(row_idx, col_idx)]
print(results)
# CSV 저장
write.csv(results, "출력/panel_varx_effects.csv", row.names = FALSE)





# 3) 내생변수가 행, 인구변수가 열
results_cells <- results %>% 
  mutate(cell = sprintf(
    "\\makecell[c]{%.3f \\\\ {\\scriptsize[%.3f, %.3f]}}",
    mean, lower, upper
  ))

results_t <- results_cells %>%
  select(variable, effect, cell) %>%
  pivot_wider(
    id_cols    = variable,
    names_from = effect,
    values_from = cell
  ) %>%
  arrange(variable)

# 3) LaTeX 파일 경로 & 출력 폴더 준비
tex_file <- "출력/panel_varx_effects_trandposed.tex"
dir.create(dirname(tex_file), recursive = TRUE, showWarnings = FALSE)

# 4) 탭ular 포맷 문자열, 헤더 준비
pop_vars <- colnames(results_t)[-1]
col_spec <- paste0("l", paste(rep("c", length(pop_vars)), collapse = ""))

lines <- c(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{makecell}",
  "\\usepackage[margin=1in]{geometry}",
  "\\begin{document}",
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Effects of Population Variables on Endogenous Variables (transposed)}\\label{tab:effects}",
  paste0("\\begin{tabular}{", col_spec, "}"),
  "\\toprule",
  paste0("Variable & ", paste(pop_vars, collapse = " & "), " \\\\"),
  "\\midrule"
)

# 5) 테이블 본문 채우기
for(i in seq_len(nrow(results_t))) {
  var_name <- results_t$variable[i]
  row_cells <- unname(results_t[i, -1])
  lines <- c(
    lines,
    paste0(var_name, " & ", paste(row_cells, collapse = " & "), " \\\\")
  )
}

# 6) footer 및 파일 쓰기
lines <- c(
  lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}",
  "\\end{document}"
)

writeLines(lines, tex_file, useBytes = TRUE)