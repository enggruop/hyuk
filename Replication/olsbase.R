  # panel_varx_table_with_stars.R
  # — 공통 VARX(1) A, B와 국가별 intercept 동시 추정 후 장기승수·p-value 계산 및 테이블 출력
  
  library(dplyr)
  library(gridExtra)
  library(grid)
  
  # 1) 데이터 불러오기 및 전처리
  df <- read.csv("C:/Users/ahnhy/Desktop/석사논문1/data.csv", stringsAsFactors = FALSE)
  df$Year <- as.Date(df$Year)
  df <- df %>% arrange(Country.ID, Year) %>%
    mutate(
      d_young   = r_young - r_imm,
      d_mid     = r_mid   - r_imm,
      d_old     = r_old   - r_imm
    ) %>%
    filter(
      !is.na(gdp_p), !is.na(cpi), !is.na(interestrate),
      !is.na(savingrate),  !is.na(unem),
      !is.na(d_young),  !is.na(d_mid),   !is.na(d_old)
    )
  
  # 2) 패널 전체에서 절편·A·B 한 번에 OLS 추정
  #    회귀식:
  #      Y_{i,t} = alpha_i + A * Y_{i,t-1} + B * W_{i,t} + epsilon_{i,t}
  #    lm(Y ~ country + . - 1) 호출로 α_i(국가별 intercept)와 A,B 동시 추정
  
  y_all <- as.matrix(select(df, gdp_p,cpi, rr, unem))
  x_all <- as.matrix(select(df, d_young, d_mid, d_old, pgr))
  Y_end_all    <- tail(y_all, -1)
  Y_lag_all    <- head(y_all, -1)
  X_end_all    <- tail(x_all, -1)
  Z_all        <- cbind(Y_lag_all, X_end_all)
  country_end  <- tail(df$Country.ID, -1)
  M <- ncol(y_all); K <- ncol(x_all)
  
  coefs_all <- matrix(NA, M, M+K,
                      dimnames=list(colnames(y_all), colnames(Z_all)))
  vcovs_all <- vector("list", M)
  for(j in seq_len(M)) {
    data_reg <- data.frame(
      Y       = Y_end_all[, j],
      country = factor(country_end),
      Z_all
    )
    fit <- lm(Y ~ country + . - 1, data = data_reg)
    cv <- coef(fit)
    coefs_all[j, ] <- cv[(length(cv) - (M+K) + 1):length(cv)]
    vcm <- vcov(fit)
    vcovs_all[[j]] <- vcm[-(1:(length(cv) - (M+K))), -(1:(length(cv) - (M+K)))]
  }
  A_hat <- coefs_all[, 1:M]
  B_hat <- coefs_all[, (M+1):(M+K)]
  InvA  <- solve(diag(M) - A_hat)
  
  # 3) 장기승수 및 p-value 계산
  dlr_base <- t(InvA %*% B_hat)
  dlr_full <- rbind(
    dlr_base,
    immigrant = -colSums(dlr_base[1:3, , drop=FALSE])
  )
  rownames(dlr_full) <- c(colnames(x_all), "immigrant")
  colnames(dlr_full) <- colnames(y_all)
  
  p_mat <- matrix(NA, nrow=K, ncol=M,
                  dimnames=list(colnames(x_all), colnames(y_all)))
  for(i in seq_len(K)) for(j in seq_len(M)) {
    vars <- sapply(seq_len(M), function(s) vcovs_all[[s]][M + i, M + i])
    se   <- sqrt(sum((InvA[j,]^2) * vars))
    p_mat[i,j] <- 2 * (1 - pnorm(abs(dlr_base[i,j] / se)))
  }
  imm_vars <- sapply(seq_len(M), function(s) vcovs_all[[s]][M + 1, M + 1])
  imm_se   <- sqrt(sum((colSums(InvA)^2) * imm_vars))
  imm_p    <- 2 * (1 - pnorm(abs(-colSums(dlr_base) / imm_se)))
  
  sig_mat <- rbind(
    ifelse(p_mat < 0.05, "**", ifelse(p_mat < 0.10, "*", "")),
    immigrant = ifelse(imm_p < 0.05, "**", ifelse(imm_p < 0.10, "*", ""))
  )
  
  tab <- matrix("", nrow=nrow(dlr_full), ncol=M,
                dimnames=list(rownames(dlr_full), colnames(dlr_full)))
  for(i in seq_len(nrow(tab))) for(j in seq_len(M)) {
    pval <- if(i <= K) p_mat[i,j] else imm_p[j]
    star <- sig_mat[i,j]
    tab[i,j] <- sprintf("%.2f%s\n(%.2f)", dlr_full[i,j], star, pval)
  }
  
  # 4) 결과 출력
  grid.newpage()
  grid.table(tab,
             rows=rownames(tab),
             theme=ttheme_minimal(
               core=list(fg_params=list(hjust=0.5,x=0.5),
                         bg_params=list(fill=rep(c("#EEEEEE","#FFFFFF"), length.out=nrow(tab)))),
               colhead=list(fg_params=list(fontface="bold",hjust=0.5,x=0.5))
             )
  )
  
