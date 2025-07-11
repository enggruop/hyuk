# ───────────────────────────────────────────────────────────────────────────────
# 필요한 패키지 로드
# ───────────────────────────────────────────────────────────────────────────────
# install.packages(c("ggplot2","dplyr","RColorBrewer","gridExtra","patchwork"))
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(patchwork)

# ───────────────────────────────────────────────────────────────────────────────
# 1) 이민자 효과 데이터 불러오기 및 정리
# ───────────────────────────────────────────────────────────────────────────────
files <- list(
  cluster1 = "C:/Users/ahnhy/Desktop/석사논문/Replication/출력/cluster_1/cluster01_LReffect.csv",
  cluster2 = "C:/Users/ahnhy/Desktop/석사논문/Replication/출력/cluster_2/cluster02_LReffect.csv",
  cluster3 = "C:/Users/ahnhy/Desktop/석사논문/Replication/출력/cluster_3/cluster03_LReffect.csv"
)
df_imm <- lapply(names(files), function(cl) {
  read.csv(files[[cl]], stringsAsFactors = FALSE) %>%
    filter(effect == "immigrant") %>%
    mutate(cluster = cl)
}) %>% bind_rows()

# ───────────────────────────────────────────────────────────────────────────────
# 2) y축 변수 순서 및 레이블 지정
# ───────────────────────────────────────────────────────────────────────────────
order_vars <- c("gdp_p","cpi","interestrate","savingrate","unem","inv")
labels_vars <- c("GDP","CPI","Interest Rate","Savings","Unemployment","Investment")
df_imm$variable <- factor(
  df_imm$variable,
  levels = order_vars,
  labels = labels_vars
)

# ───────────────────────────────────────────────────────────────────────────────
# 3) 색상 팔레트 준비
# ───────────────────────────────────────────────────────────────────────────────
pal3 <- brewer.pal(3, "Set2")
names(pal3) <- c("cluster1","cluster2","cluster3")

# ───────────────────────────────────────────────────────────────────────────────
# 4) Forest plot 생성
# ───────────────────────────────────────────────────────────────────────────────
p_forest <- ggplot(df_imm, aes(x = mean, y = variable, color = cluster)) +
  # 배경 그리드 제거
  theme_classic(base_size = 14) +
  theme(
    panel.grid       = element_blank(),
    plot.title       = element_text(face="bold", hjust=0.5),
    axis.text        = element_text(size=12),
    axis.title.x     = element_text(face="bold", size=12),
    legend.position  = "right",
    panel.border     = element_rect(fill=NA, color="black")
  ) +
  # 변수 사이에 점선 추가
  geom_hline(
    yintercept = seq(1.5, length(labels_vars)-0.5, by = 1),
    linetype   = "dotted",
    color      = "grey80"
  ) +
  # 0 기준 실선
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.6) +
  # 점과 오차 막대
  geom_point(position = position_dodge(0.6), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(0.6), height = 0.2, size=0.8) +
  # y축 순서 뒤집기 (GDP 위)
  scale_y_discrete(limits = rev(labels_vars)) +
  # 색상 및 범례
  scale_color_manual(
    values = pal3,
    labels = c("Cluster 1","Cluster 2","Cluster 3"),
    name   = "Cluster"
  ) +
  labs(
    title = "Immigrant Effect on Macroeconomic Variables by Cluster",
    x     = "Estimated Coefficient (90% CI)",
    y     = NULL
  )

# ───────────────────────────────────────────────────────────────────────────────
# 5) 클러스터 특성표 생성
# ───────────────────────────────────────────────────────────────────────────────
tbl <- data.frame(
  Cluster     = c("Cluster 1","Cluster 2","Cluster 3"),
  Dependency  = c("Low","High","High"),
  Immigration = c("Low","Low","High"),
  stringsAsFactors = FALSE
)
tbl_grob <- tableGrob(
  tbl, rows = NULL,
  theme = ttheme_default(
    core    = list(fg_params = list(hjust=0, x=0)),
    colhead = list(fg_params = list(fontface="bold"))
  )
)

# ───────────────────────────────────────────────────────────────────────────────
# 6) Forest plot 아래에 표 배치 및 저장
# ───────────────────────────────────────────────────────────────────────────────
final_plot <- p_forest / tbl_grob + plot_layout(heights = c(3,1))

# 출력
print(final_plot)

# 저장
ggsave(
  filename = "immigrant_effect_by_cluster_with_table.png",
  plot     = final_plot,
  path     = "C:/Users/ahnhy/Desktop/석사논문/Replication/출력",
  width    = 8, height = 8, dpi = 300
)
