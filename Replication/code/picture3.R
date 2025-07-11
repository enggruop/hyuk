# ───────────────────────────────────────────────────────────────────────────────
# 1) 필요한 패키지 로드
# ───────────────────────────────────────────────────────────────────────────────
# install.packages(c("ggplot2","viridis","dplyr","viridisLite"))
library(ggplot2)
library(viridisLite)   # turbo(n) 색상 생성
library(dplyr)         # 데이터 조작

# ───────────────────────────────────────────────────────────────────────────────
# 2) 작업 디렉토리 설정 및 데이터 불러오기
# ───────────────────────────────────────────────────────────────────────────────
setwd("C:/Users/ahnhy/Desktop/석사논문/Replication")
df <- read.csv("data/cleaned_data/panel.csv", stringsAsFactors = FALSE)

# ───────────────────────────────────────────────────────────────────────────────
# 3) 파생변수 생성 및 NA 제거
# ───────────────────────────────────────────────────────────────────────────────
df <- df %>%
  mutate(
    dependency_ratio = (r_young + r_old) / r_mid,
    label            = paste0(sprintf("%02d", Year %% 100), Country.ID)
  ) %>%
  filter(!is.na(dependency_ratio), !is.na(r_imm))

# 라벨링할 연도 지정
label_years <- c(1990, 2000, 2010, 2015, 2020, 2024)
df_labels   <- filter(df, Year %in% label_years)

# ───────────────────────────────────────────────────────────────────────────────
# 4) 색상 벡터 생성 (국가 수만큼 turbo 팔레트)
# ───────────────────────────────────────────────────────────────────────────────
n_country    <- n_distinct(df$Country.ID)
country_cols <- turbo(n_country)

# ───────────────────────────────────────────────────────────────────────────────
# 5) 기본 산점도 객체 p 생성
# ───────────────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(
  x     = dependency_ratio,
  y     = r_imm,
  color = Country.ID,
  alpha = Year
)) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_text(
    data        = df_labels,
    aes(label    = label),
    vjust       = -0.5,
    size        = 2,
    show.legend = FALSE
  ) +
  scale_color_manual(values = country_cols, name = "Country Code") +
  scale_alpha_continuous(range = c(0.3, 1), name = "Yeara") +
  labs(
    title = "Dependency and Immigration Ratio by Country (1990–2024)",
    x     = "Dependency Ratio",
    y     = "Immigration Ratio"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title         = element_text(face = "bold", size = 12),
    axis.text          = element_text(size = 10),
    panel.border       = element_rect(color = "black", fill = NA, size = 0.5),
    legend.background  = element_rect(fill = NA, color = "grey70", size = 0.5),
    legend.key         = element_rect(fill = NA, color = NA),
    legend.key.size    = unit(0.6, "cm"),
    legend.key.width   = unit(0.6, "cm"),
    legend.title       = element_text(face = "bold", size = 11, hjust = 0.5),
    legend.title.align = 0.5,
    legend.text        = element_text(size = 9),
    legend.spacing.y   = unit(0.2, "cm"),
    legend.margin      = margin(4, 4, 4, 4)
  )

# ───────────────────────────────────────────────────────────────────────────────
# 6) 범례 레이아웃 적용 및 최종 플롯 p2 생성
# ───────────────────────────────────────────────────────────────────────────────
p2 <- p +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust    = 0.5,
      ncol           = 2,
      byrow          = TRUE,
      keywidth       = unit(0.6, "cm"),
      keyheight      = unit(0.6, "cm")
    )
  ) +
  theme(
    legend.position     = "right",
    legend.justification= c(0, 1)
  )

# ───────────────────────────────────────────────────────────────────────────────
# 7) 플롯 출력 및 저장
# ───────────────────────────────────────────────────────────────────────────────
print(p2)

ggsave(
  filename = "country_scatter_styled.png",
  plot     = p2,
  path     = "C:/Users/ahnhy/Desktop/석사논문/Replication/출력",
  width    = 7,
  height   = 5,
  dpi      = 300
)
