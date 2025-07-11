# 필요한 패키지
library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)
setwd("C:/Users/ahnhy/Desktop/석사논문/Replication")
# 1) 데이터 불러오기 및 파생
df <- read.csv("data/cleaned_data/panel.csv", stringsAsFactors = FALSE) %>%
  mutate(
    dependency_ratio = (r_young + r_old) / r_mid,
    label            = paste0(sprintf("%02d", Year %% 100), Country.ID)
  ) %>%
  filter(!is.na(dependency_ratio), !is.na(r_imm))

# 2) K-means 클러스터링 (K=3)
K <- 3
clust_data   <- df[, c("dependency_ratio", "r_imm")]
clust_scaled <- scale(clust_data)
set.seed(123)
km <- kmeans(clust_scaled, centers = K, nstart = 25)
df$cluster <- factor(km$cluster)

# 3) 중심점 역표준화
centers <- as.data.frame(km$centers)
centers <- sweep(centers, 2, attr(clust_scaled, "scaled:scale"), `*`)
centers <- sweep(centers, 2, attr(clust_scaled, "scaled:center"), `+`)
centers$cluster <- factor(1:K)
# 범례에 쓸 레이블: 좌표 소수점 둘째 자리까지
legend_labels <- with(centers,
                      sprintf("(%.2f, %.2f)", dependency_ratio, r_imm)
)
names(legend_labels) <- centers$cluster

# 4) 연도 라벨용 데이터
label_years <- c(1990,2000,2010,2015,2020,2024)
df_labels <- df %>% filter(Year %in% label_years)

# 5) 색상 팔레트
palette <- brewer.pal(K, "Set2")

# 6) 시각화
p <- ggplot(df, aes(x = dependency_ratio, y = r_imm, color = cluster)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_text(
    data = df_labels,
    aes(label = label),
    color = "black", size = 3, vjust = -0.5
  ) +
  geom_point(
    data = centers,
    aes(x = dependency_ratio, y = r_imm),
    shape = 21, fill = "red", color = "black",
    size = 4, stroke = 1.2, inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = palette,
    labels = legend_labels,   # ★ 여기에서 범례 레이블을 좌표 문자열로 바꿉니다
    name   = "Cluster center\n(Dependency, Immig.)"
  ) +
  labs(
    title = "Clustering of Countries by Dependency and Immigration Ratio",
    x     = "Dependency Ratio",
    y     = "Immigration Ratio"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5),
    legend.position    = c(0.85, 0.15),
    legend.background  = element_blank(),
    legend.key         = element_rect(fill = NA),
    axis.title         = element_text(face = "bold", size = 12),
    axis.text          = element_text(size = 11),
    panel.border       = element_rect(color = "black", fill = NA, size = 0.5)
  )
p + theme(
  # 범례 박스 스타일
  legend.background   = element_rect(fill = NA, color = "grey70", size = 0.5),
  legend.key          = element_rect(fill = NA, color = NA),
  legend.key.size     = unit(0.6, "cm"),
  legend.key.width    = unit(0.6, "cm"),
  # 범례 제목
  legend.title        = element_text(face = "bold", size = 12, hjust = 0.5),
  legend.title.align  = 0.5,
  # 범례 항목
  legend.text         = element_text(size = 10),
  legend.spacing.y    = unit(0.3, "cm"),
  legend.margin       = margin(6, 6, 6, 6),
  # 범례 위치 (예: 그래프 바깥 오른쪽 아래)
  legend.position     = c(1.02, 0.2),
  legend.justification= c(0, 0),
  # 기타 테마 정리
  plot.title          = element_text(face = "bold", size = 16, hjust = 0.5),
  axis.title          = element_text(face = "bold", size = 12),
  axis.text           = element_text(size = 11),
  panel.border        = element_rect(color = "black", fill = NA, size = 0.5)
)
print(p)

# 7) 저장
ggsave(
  filename = "cluster_plot_with_centers_legend.png",
  plot     = p,
  path     = "C:/Users/ahnhy/Desktop/석사논문/Replication/출력",
  width    = 6,
  height   = 5,
  dpi      = 300
)

cluster_summary <- df %>%
  arrange(cluster, Country.ID, Year) %>%
  group_by(cluster, Country.ID) %>%
  mutate(
    new_run = if_else(is.na(lag(Year)) | Year - lag(Year) != 1, TRUE, FALSE),
    run_id  = cumsum(new_run)
  ) %>%
  group_by(cluster, Country.ID, run_id) %>%
  summarise(
    start = first(Year),
    end   = last(Year),
    .groups = "drop"
  ) %>%
  mutate(
    range_label = paste0(
      sprintf("%02d", start %% 100), "-",
      sprintf("%02d", end   %% 100),
      Country.ID
    )
  ) %>%
  group_by(cluster) %>%
  summarise(
    elements = paste(range_label, collapse = "; "),
    .groups  = "drop"
  )

write.csv(cluster_summary,
          file         = "cluster_summary.csv",
          row.names    = FALSE,
          fileEncoding = "UTF-8") 
