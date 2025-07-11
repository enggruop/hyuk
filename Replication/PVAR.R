# my_panel_varx_with_demo.R
# — panel VARX(1) with country fixed effects +
#    외생(exo + pgr) vs. 인구(demo) 분리 →
#    장기승수 + p-값(white2 HC3) → grid.table 출력

### 0) 라이브러리 로드 (sandwich → plm 순서 중요)
library(dplyr)
library(lmtest)
library(sandwich)
library(plm)         # sandwich 다음에 로드해서 plm::vcovHC 메소드가 등록되도록
library(Matrix)      # bdiag() 용
library(gridExtra)
library(grid)

### 1) 데이터 로드 & 전처리
df <- read.csv("C:/Users/ahnhy/Desktop/석사논문1/data.csv",
               stringsAsFactors = FALSE)
df$Year <- as.Date(df$Year)
df <- df %>%
  arrange(Country.ID, Year) %>%
  mutate(
    d_young = r_young - r_imm,
    d_mid   = r_mid   - r_imm,
    d_old   = r_old   - r_imm
  ) %>%
  filter(complete.cases(
    gdp_p, cpi, interestrate,
    savingrate, unem,
    d_young, d_mid, d_old,
    pgr
  ))
pdf <- pdata.frame(df, index = c("Country.ID","Year"))

### 2) 변수 목록 정의
y_vars   <- c("gdp_p", "cpi", "rr", "unem",'inv','savingrate')
exo_vars <- c("pgr","logoil")
demo_vars<- c("d_young","d_mid","d_old")
lag_terms<- paste0(y_vars, "_lag")

### 3) lag(Y) 생성
for(i in seq_along(y_vars)) {
  pdf[[ lag_terms[i] ]] <- lag(pdf[[ y_vars[i] ]], 1)
}

### 4) formula 생성 & plm 추정
formulae <- lapply(seq_along(y_vars), function(i) {
  as.formula(
    paste0(
      y_vars[i], " ~ ",
      lag_terms[i], " + ",
      paste(c(exo_vars, demo_vars), collapse = " + ")
    )
  )
})
names(formulae) <- y_vars
plm_models <- lapply(formulae, function(f) {
  plm(f, data = pdf, model = "within", effect = "individual")
})

### 5) 계수·공분산 준비
M     <- length(y_vars)
K_exo <- length(exo_vars)
K_dem <- length(demo_vars)

A_hat <- matrix(0, M, M,         dimnames = list(y_vars, y_vars))
B_exo <- matrix(0, M, K_exo,     dimnames = list(y_vars, exo_vars))
D_hat <- matrix(0, M, K_dem,     dimnames = list(y_vars, demo_vars))
vcovs  <- vector("list", M)

for(i in seq_len(M)) {
  cf          <- coef(plm_models[[i]])
  A_hat[i,i]  <- cf[ lag_terms[i] ]
  B_exo[i,]   <- cf[ exo_vars ]
  D_hat[i,]   <- cf[ demo_vars ]
  vcovs[[i]]  <- vcovHC(plm_models[[i]],
                        method = "white1",
                        type   = "HC3")
}

### 6) 장기승수 계산
InvA   <- solve(diag(M) - A_hat)
long_B <- InvA %*% B_exo    # 외생 변수 장기승수
long_D <- InvA %*% D_hat    # 인구충격 장기승수

dlr_base <- t(cbind(long_B, long_D))
rownames(dlr_base) <- c(exo_vars, demo_vars)
colnames(dlr_base) <- y_vars

### 7) p-값 & 별표 계산 함수 (gradnum 없이 vcovs 만 사용)
computePValues <- function(InvA, dlr_base, vcovs, exo_vars, demo_vars) {
  M        <- ncol(dlr_base)
  K_exo    <- length(exo_vars)
  K_dem    <- length(demo_vars)
  all_vars <- c(exo_vars, demo_vars)
  
  # 1) exo + demo 개별 p-값
  p_mat <- matrix(NA, length(all_vars), M,
                  dimnames = list(all_vars, colnames(dlr_base)))
  for(i in seq_along(all_vars)) {
    var_name <- all_vars[i]
    pos      <- which(names(coef(plm_models[[1]])) == var_name)
    for(j in seq_len(M)) {
      vars_j     <- sapply(vcovs, function(V) V[pos, pos])
      se_j       <- sqrt(sum((InvA[j, ]^2) * vars_j))
      p_mat[i,j] <- 2 * (1 - pnorm(abs(dlr_base[i,j] / se_j)))
    }
  }
  
  # 2) immigrant p-값
  demo_idx  <- (K_exo + 1):(K_exo + K_dem)
  vars_demo <- sapply(vcovs, function(V) sum(V[demo_idx, demo_idx]))
  p_imm     <- numeric(M)
  names(p_imm) <- colnames(dlr_base)
  for(j in seq_len(M)) {
    m_imm_j    <- -sum(dlr_base[demo_idx, j])
    se_imm_j   <- sqrt(sum((InvA[j, ]^2) * vars_demo))
    p_imm[j]   <- 2 * (1 - pnorm(abs(m_imm_j / se_imm_j)))
  }
  
  list(p_mat = p_mat, imm_p = p_imm)
}

# 호출 예시
p_vals <- computePValues(
  InvA, dlr_base, vcovs,
  exo_vars  = exo_vars,
  demo_vars = demo_vars
)

### 8) 전체 long‐run + immigrant 결합
demo_idx <- (K_exo + 1):(K_exo + K_dem)
dlr_full <- rbind(
  dlr_base,
  immigrant = -colSums(
    dlr_base[demo_idx, , drop = FALSE]
  )
)

### 9) 그리드테이블 포맷 & 출력
formatTable <- function(dlr_full, p_vals, alpha = 0.05) {
  K   <- nrow(p_vals$p_mat)
  M   <- ncol(dlr_full)
  sig <- rbind(
    ifelse(p_vals$p_mat < alpha, "**", ""),
    immigrant = ifelse(p_vals$imm_p < alpha, "**", "")
  )
  tab <- matrix("", nrow = nrow(dlr_full), ncol = M,
                dimnames = dimnames(dlr_full))
  for(i in seq_len(nrow(tab))) for(j in seq_len(M)) {
    pv   <- if(i <= K) p_vals$p_mat[i,j] else p_vals$imm_p[j]
    star <- sig[i,j]
    tab[i,j] <- sprintf("%.2f%s\n(%.2f)",
                        dlr_full[i,j], star, ifelse(is.na(pv), NA, pv)
    )
  }
  tab
}
tab <- formatTable(dlr_full, p_vals)

grid.newpage()
grid.table(tab,
           rows = rownames(tab),
           theme = ttheme_minimal(
             core   = list(
               fg_params = list(hjust=0.5, x=0.5),
               bg_params = list(fill = rep(c("#EEEEEE","#FFFFFF"),
                                           length.out = nrow(tab)))
             ),
             colhead = list(fg_params = list(fontface="bold", hjust=0.5, x=0.5))
           )
)
