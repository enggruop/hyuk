# install.packages(c("ggplot2", "RColorBrewer"))
#install.packages("Cairo")
library(Cairo)
library(ggplot2)
library(RColorBrewer)

# 1) 워킹 디렉토리 & 데이터 불러오기
setwd("C:/Users/ahnhy/Desktop/석사논문/Replication")
df <- read.csv("출력/panel_varx_effects_nooil.csv", stringsAsFactors = FALSE)

# 2) pop_g 제거
df <- subset(df, effect != "pop_g")

# 3) y축 변수 레이블·순서 지정 & 뒤집어서 GDP가 맨 위에
df$variable <- factor(
  df$variable,
  levels = c("gdp_p", "cpi", "interestrate", "savingrate", "unem", "inv"),
  labels = c("GDP", "CPI", "Interest Rate", "Savings", "Unemployment", "Investment")
)
df$variable <- factor(df$variable, levels = rev(levels(df$variable)))

# 4) effect 레이블 순서 지정 (Young이 범례 제일 위로)
df$effect <- factor(
  df$effect,
  levels = c("young", "mid", "old", "immigrant"),
  labels = c("Young", "Middle", "Old", "Immigrant")
)

# 5) y축 변수 사이마다 그을 점선 위치 계산
n_vars   <- length(levels(df$variable))
hline_pos <- seq(1.5, n_vars - 0.5, by = 1)

# 6) 색상 팔레트 준비
pal <- brewer.pal(n = length(levels(df$effect)), name = "Dark2")

# 7) 플롯 생성
p <- ggplot(df, aes(x = mean, y = variable, color = effect)) +
  # (1) y축 경계 점선
  geom_hline(
    yintercept = hline_pos,
    linetype   = "dotted",
    color      = "grey60",
    size       = 0.5
  ) +
  # (2) 수평 에러바
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height     = 0.2,
    position   = position_dodge(width = 0.6),
    size       = 1
  ) +
  # (3) 추정점
  geom_point(
    position = position_dodge(width = 0.6),
    size     = 3
  ) +
  # (4) 영 기준선
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  # (5) 색상 및 범례 순서 고정
  scale_color_manual(
    values = pal,
    breaks = c("Young", "Middle", "Old", "Immigrant")
  ) +
  # (6) 레이블
  labs(
    title = "Population Effects on Macroeconomic Variables",
    x     = "Coefficient Estimate (with 95% CI)",
    y     = NULL,
    color = "Population Effect"
  ) +
  # (7) 학술지 스타일 테마 + 범례를 오른쪽으로 밀기
  theme_bw(base_size = 14, base_family = "serif") +
  theme(
    plot.title          = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position     = c(0.85, 0.15),
    legend.justification= c(0, 0.5),
    legend.background   = element_blank(),
    legend.key          = element_blank(),
    legend.title        = element_text(face = "bold", size = 12),
    legend.text         = element_text(size = 11),
    axis.text           = element_text(size = 12),
    axis.title.x        = element_text(size = 13, margin = margin(t = 8)),
    panel.grid.major    = element_blank(),
    panel.grid.minor    = element_blank(),
    axis.ticks          = element_line(size = 0.5)
)

# 9) PNG로 저장

CairoPNG(
  filename = "출력/panel_varx_forest_plot1.png",
  width    = 13,       # 인치 단위
  height   = 6,       # 인치 단위
  units    = "in",
  res      = 300      # 해상도
)
print(p)
