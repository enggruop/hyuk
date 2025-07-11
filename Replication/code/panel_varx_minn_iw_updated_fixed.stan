data {
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
}
