# panel_varx_minn_iw_updated.R
# VARX(p) with Minnesota prior, differencing relative to immigrant baseline (r_imm)

# 1) 설치 및 로드
required_pkgs <- c('dplyr','rstan','posterior','rstudioapi')
for (pkg in required_pkgs) if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg)
library(dplyr)
library(rstan)
library(posterior)

# rstan 옵션
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 2) 데이터 불러오기·전처리
df <- read.csv('C:/Users/ahnhy/Desktop/석사논문/이민자 제외/인구구조.csv', stringsAsFactors=FALSE)
df$Year <- as.Date(df$Year)
df <- df %>%
  arrange(Country.ID, Year) %>%
  group_by(Country.ID) %>%
  mutate(
    # 차분 변수 정의: 모두 r_imm 대비 (r_old는 원본 데이터 사용)
    d_young = r_young - r_imm,
    d_mid   = r_mid   - r_imm,
    d_old   = r_old   - r_imm,
    # 예시: 저축률 차분
    dsaving = savingrate - lag(savingrate)
  ) %>%
  filter(!is.na(dsaving)) %>%
  ungroup()

# 3) 행렬 생성 및 NA 제거
y_mat <- as.matrix(select(df, grgdp_p, cpi, interestrate, dsaving, unem))
x_mat <- as.matrix(select(df, d_young, d_mid, d_old))
keep  <- complete.cases(y_mat, x_mat)
y_mat <- y_mat[keep,]; x_mat <- x_mat[keep,]
N <- nrow(y_mat); M <- ncol(y_mat); K <- ncol(x_mat); p <- 1

# 4) 하이퍼파라미터 설정
lambda1 <- 0.3; lambda2 <- 0.5; lambda3 <- 1; decay <- 1.0
df_iw <- M + 1; Psi_iw <- diag((0.02)^2, M)

# 5) Stan 모델 생성 (.stan 파일)
stan_code <- "
data {
  int<lower=1> N; int<lower=1> M; int<lower=1> K; int<lower=1> p;
  matrix[N, M] Y;
  matrix[N, K] X;
  real<lower=0> lambda1;
  real<lower=0> lambda2;
  real<lower=0> lambda3;
  real<lower=0> decay;
  int<lower=M+1> df_iw;
  matrix[M, M] Psi_iw;
}
parameters {
  matrix[M, M] A[p];
  matrix[K, M] B;
  vector[M] alpha;
  cov_matrix[M] Sigma;
}
model {
  // Minnesota prior on A
  for (lag in 1:p)
    for (i in 1:M) for (j in 1:M) {
      real sdA = lambda1 / pow(lag, decay) * (i==j ? 1 : lambda2);
      A[lag][i,j] ~ normal((i==j && lag==1) ? 1 : 0, sdA);
    }
  // Minnesota prior on B
  for (k in 1:K) for (i in 1:M) {
    B[k,i] ~ normal(0, lambda1 * lambda3);
  }
  alpha ~ normal(0, 1);
  Sigma ~ inv_wishart(df_iw, Psi_iw);
  for (t in (p+1):N) {
    vector[M] mu = alpha;
    for (lag in 1:p) mu += A[lag] * to_vector(Y[t-lag,]);
    mu += B' * to_vector(X[t,]);
    Y[t]' ~ multi_normal(mu, Sigma);
  }
}
"
writeLines(stan_code, 'panel_varx_minn_iw_updated.stan')

# 6) 컴파일 & 샘플링
target_model <- stan_model(model_code = stan_code, auto_write=TRUE)
stan_data <- list(
  N = N, M = M, K = K, p = p,
  Y = y_mat, X = x_mat,
  lambda1 = lambda1, lambda2 = lambda2,
  lambda3 = lambda3, decay = decay,
  df_iw = df_iw, Psi_iw = Psi_iw
)
fit <- sampling(
  target_model, data = stan_data,
  chains = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta=0.9)
)

# 7) 결과 요약
print(fit, pars = c('A','B','Sigma'))

# 8) Exogenous long-run multiplier 계산
a_arr <- rstan::extract(fit, 'A')$A  # draws x p x M x M
b_arr <- rstan::extract(fit, 'B')$B  # draws x K x M
S <- dim(a_arr)[1]
DLR_samples <- array(NA, dim = c(S, K, M))
for (s in 1:S) {
  A_s <- a_arr[s,1,,]
  B_s <- b_arr[s,,]
  InvA_s <- solve(diag(M) - A_s)
  DLR_samples[s,,] <- t(InvA_s %*% t(B_s))
}
# 요약
dlr_mean  <- apply(DLR_samples, c(2,3), mean)
dlr_lower <- apply(DLR_samples, c(2,3), quantile, probs=0.025)
dlr_upper <- apply(DLR_samples, c(2,3), quantile, probs=0.975)

# r_imm 복원 효과: d_young + d_mid + d_old 합의 음수
imm_mean  <- -rowSums(dlr_mean)
imm_lower <- -rowSums(dlr_upper)
imm_upper <- -rowSums(dlr_lower)

# 데이터프레임 생성 및 출력
library(tibble)
DLR_df <- expand.grid(
  Exog  = c('d_young','d_mid','d_old'),
  Endog = colnames(y_mat)
) %>%
  mutate(
    Mean  = as.vector(dlr_mean),
    Lower = as.vector(dlr_lower),
    Upper = as.vector(dlr_upper),
    Sig   = ifelse(dlr_lower * dlr_upper > 0, 'Significant','Not significant')
  )
# immigrant 복원 행 추가
imm_df <- tibble(
  Exog  = 'r_imm',
  Endog = colnames(y_mat),
  Mean  = imm_mean,
  Lower = imm_lower,
  Upper = imm_upper,
  Sig   = ifelse(imm_lower * imm_upper > 0, 'Significant','Not significant')
)
DLR_df <- bind_rows(DLR_df, imm_df)

cat('\n==== Exogenous Long-Run Multiplier Summary (with 95% CI) ====' ,'\n')
print(DLR_df)
