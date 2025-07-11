# panel_varx_table_with_stars.R
# — 국가별 VARX(1) OLS 회귀 → 장기승수(DLR)·p-value 계산 → R 플롯 창에 테이블 출력
# — 행: young, mid, old, immigrant
# — 열: 내생변수명 (lag_ 접두어 제거), 값 위: 추정치+별표, 아래: p-value (괄호)

library(dplyr)
library(gridExtra)
library(grid)

# 전체 데이터 불러오기
df_all <- read.csv("C:/Users/ahnhy/Desktop/석사논문1/data.csv",
                   stringsAsFactors = FALSE)
df_all$Year <- as.Date(df_all$Year)

# 국가별로 순차적 테이블 출력
for(country in unique(df_all$Country.ID)) {
  # 데이터 전처리
  df <- df_all %>%
    filter(Country.ID == country) %>%
    arrange(Year) %>%
    mutate(
      d_young = r_young - r_imm,
      d_mid   = r_mid   - r_imm,
      d_old   = r_old   - r_imm
    ) %>%
    filter(
      !is.na(grgdp_p), !is.na(cpi), !is.na(interestrate),
      !is.na(savingrate), !is.na(unem),
      !is.na(d_young), !is.na(d_mid), !is.na(d_old)
    )
  
  # VARX(1) OLS 준비
  y_mat <- as.matrix(select(df, grgdp_p, cpi, interestrate, savingrate, unem))
  x_mat <- as.matrix(select(df, d_young, d_mid, d_old))
  keep  <- complete.cases(y_mat, x_mat)
  y_mat <- y_mat[keep, ]; x_mat <- x_mat[keep, ]
  N      <- nrow(y_mat)
  Y_lag  <- head(y_mat, -1);
  Y_end  <- tail(y_mat, -1);
  X_end  <- tail(x_mat, -1);
  colnames(Y_lag) <- paste0("lag_", colnames(y_mat))
  colnames(X_end) <- colnames(x_mat)
  Z <- cbind(Y_lag, X_end)
  M <- ncol(y_mat); K <- ncol(x_mat)
  
  # 회귀 및 패러미터 수집
  # … (위 부분은 그대로)
  
  # 3) 회귀 및 패러미터 수집 (절편 제거)
  coefs <- array(NA, dim=c(M, M+K),
                 dimnames=list(colnames(y_mat), c(colnames(Y_lag), colnames(X_end))))
  vcovs <- vector("list", M)
  for(j in seq_len(M)){
    # 절편 없이 회귀: "~ Z - 1"
    fit <- lm(Y_end[, j] ~ Z - 1)
    # 이제 coef(fit)에 절편이 없으므로 바로 사용
    coefs[j, ]   <- coef(fit)
    # vcov(fit)에도 절편 row/col이 없으므로 그대로 사용
    vcovs[[j]]   <- vcov(fit)
  }
  A_hat <- coefs[, colnames(Y_lag)]
  B_hat <- coefs[, colnames(X_end)]
  
  # … (이하 p-value 계산, 테이블 생성, 출력 부분도 그대로)
  
  
  # 장기승수 및 p-value 계산 (Delta method)
  InvA    <- solve(diag(M) - A_hat)
  DLR_mean<- t(InvA %*% B_hat)
  p_mat   <- matrix(NA, nrow=K, ncol=M,
                    dimnames=list(rownames(DLR_mean), colnames(DLR_mean)))
  for(i in seq_len(K)) for(j in seq_len(M)){
    vars <- sapply(seq_len(M), function(s) vcovs[[s]][M + i, M + i])
    se_ij <- sqrt(sum((InvA[j,]^2)*vars))
    p_mat[i,j] <- 2*(1 - pnorm(abs(DLR_mean[i,j]/se_ij)))
  }
  
  # r_imm 복원 승수
  imm_mean <- -colSums(DLR_mean)
  imm_p    <- 2*(1 - pnorm(abs(imm_mean / sqrt(colSums(p_mat^2)))))
  DLR_full <- rbind(DLR_mean, immigrant=imm_mean)
  p_full   <- rbind(p_mat,    immigrant=imm_p)
  
  # 테이블 문자열 생성
  sig_full <- ifelse(p_full<0.05, "**", ifelse(p_full<0.10, "*", ""))
  tab <- matrix("", nrow=nrow(DLR_full), ncol=ncol(DLR_full),
                dimnames=list(c("young","mid","old","immigrant"),
                              gsub("^lag_","", colnames(DLR_full))))
  for(i in seq_len(nrow(tab))) for(j in seq_len(ncol(tab))) {
    tab[i,j] <- sprintf("%.2f%s\n(%.2f)",
                        DLR_full[i,j], sig_full[i,j], p_full[i,j])
  }
  
  # 국가명 타이틀
  grid.newpage()
  grid.text(sprintf("Country: %s", country), y=unit(0.98, "npc"), gp=gpar(fontsize=14, fontface="bold"))
  # 테이블 출력
  grid.table(
    tab,
    rows = rownames(tab),
    theme = ttheme_minimal(
      core = list(fg_params=list(hjust=0.5, x=0.5),
                  bg_params=list(fill=rep(c("#EEEEEE","#FFFFFF"), length.out=nrow(tab)))),
      colhead = list(fg_params=list(fontface="bold", hjust=0.5, x=0.5))
    )
  )
} 