library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(RColorBrewer)

# 데이터 로드
df <- read_csv("C:/Users/ahnhy/Desktop/석사논문/Replication/data/cleaned_data/panel.csv")

# 1990년 이후 평균 계산 및 long 포맷 전환
df_ts <- df %>%
  filter(Year >= 1990) %>%
  group_by(Year) %>%
  summarise(
    Young     = mean(r_young, na.rm = TRUE),
    Middle    = mean(r_mid,    na.rm = TRUE),
    Old       = mean(r_old,    na.rm = TRUE),
    Immigrant = mean(r_imm,    na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -Year, names_to = "Group", values_to = "Share")

# 같은 Dark2 팔레트 쓰기
# brewer.pal(4,"Dark2")  → c("#1B9E77","#D95F02","#7570B3","#E7298A")

ggplot(df_ts, aes(Year, Share, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      Young     = "#1B9E77",  # 녹색
      Middle    = "#D95F02",  # 주황
      Old       = "#7570B3",  # 보라
      Immigrant = "#E7298A"   # 분홍
    ),
    guide  = guide_legend(override.aes = list(size = 4))  # 점 크기 키우기
  ) +
  labs(
    title = "Population Shares by Group (1990–2024)",
    x     = "Year",
    y     = "Average Share",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
