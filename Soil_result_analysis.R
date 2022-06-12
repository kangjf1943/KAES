# Statement ---- 
# analyze the between-group variance by: 
# productive green space vs. non-productive green space 
# different wards
# different soil types 

# Package ----
library(shapefiles)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(showtext)
showtext_auto()

# Function ----

# Read data ----
# read Kyoto farmland shapefile after first filter 
frmlnd.ward.soil <- read.shapefile("GProcData/Frmlnd_filter_add_ward_soil") %>% 
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  rename_with(tolower) %>% 
  select(plotid, ward, soilname11) %>% 
  rename(soil = soilname11) %>% 
  mutate(plotid = as.character(plotid)) %>% 
  # 将土壤改成小写，且修正拼写错误
  mutate(soil = tolower(soil)) %>% 
  mutate(soil = case_when(
    soil == "residual regosols" ~ "Residual regosol", 
    soil == "brown forest soils" ~ "Brown forest soil", 
    soil == "gray lowland soils" ~ "Gray lowland soil", 
    soil == "blown lowland soils" ~ "Brown lowland soil", 
    soil == "yellow soils" ~ "Yellow soil", 
    soil == "gley soils" ~ "Gley soil", 
    soil == "brown forest soils (dry)" ~ "Brown forest soil (dry)", 
    soil == "a" ~ "Undefined"
  ))

# read Kyoto productive green space shapefile of 2017
prod.grn.sp.17 <- read.shapefile("GProcData/Kyoto_prod_green_space_2017") %>% 
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  rename_with(tolower) %>% 
  select(plotid) %>% 
  mutate(plotid = as.integer(plotid), 
         prod_grn_sp = "yes") %>% 
  mutate(plotid = as.character(plotid))

# soil test results 
soil.result.ha <- 
  read.xlsx("RRawData/Soil_test_result.xlsx", sheet = "HaData") %>% 
  as_tibble() %>% 
  select(google_id, surface, p_time_1_fin, p_time_2_fin, p_time_3_fin) %>% 
  rename(time_1 = p_time_1_fin, time_2 = p_time_2_fin, time_3 = p_time_3_fin) %>% 
  mutate(google_id = as.character(google_id), 
         surface = as.character(surface))
soil.result.ta <- 
  read.xlsx("RRawData/Soil_test_result.xlsx", sheet = "TaData") %>% 
  as_tibble() %>% 
  select(gis_id, depth_1, depth_2, depth_3) %>% 
  rename(plotid = gis_id) %>% 
  # 计算平均深度
  mutate(mean_depth_cm = (depth_1 + depth_2 + depth_3)/3) %>% 
  mutate(plotid = as.character(plotid), 
         mean_depth_cm = as.numeric(gsub("-", NA, mean_depth_cm)))

# process soil sampling investigation data 
# function: process soil sample plot data
# parameter: 
# path.soil.id: path to the file
# frm.type: farmland type, "ta" or "ha" 
GetMatchGG <- function(path.soil.id, frm.type) {
  read.csv(path.soil.id) %>% 
    rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>% 
    as_tibble() %>% 
    rename(plotid = gis_id) %>% 
    select(plotid, google_id) %>% 
    mutate(type = frm.type) %>% 
    mutate(plotid = as.character(plotid), 
           google_id = as.character(google_id)) %>% 
    return()
}

match.gg.ta <- GetMatchGG("RRawData/Ta_google_to_gis.csv", frm.type = "ta")
match.gg.ha <- GetMatchGG("RRawData/Ha_google_to_gis.csv", frm.type = "ha")

ProcSmpData <- function(soil.result, match.gg) {
  soil.result %>% 
    left_join(match.gg, by = "google_id") %>% 
    left_join(frmlnd.ward.soil, by = "plotid") %>% 
    left_join(prod.grn.sp.17, by = "plotid") %>% 
    mutate(prod_grn_sp = ifelse(is.na(prod_grn_sp), "no", prod_grn_sp)) %>% 
    return()
}

smp.ha <- ProcSmpData(soil.result.ha, match.gg.ha) %>% 
  # 重排各区顺序
  mutate(ward = factor(
    ward, levels = c("北区", "伏見区", "南区", "中京区", "西京区", "山科区", 
                     "左京区", "下京区", "右京区", "東山区", "上京区"))) %>% 
  # 添加分区合并信息
  mutate(ward_agg = case_when(
    ward == "上京区" ~ "中部",
    ward == "中京区" ~ "中部",
    ward == "下京区" ~ "中部",
    ward == "南区" ~ "中部",
    ward == "東山区" ~ "中部",
    TRUE ~ as.character(ward)
  ))
smp.ha.long <- smp.ha %>% 
  pivot_longer(cols = c(time_1, time_2, time_3), 
               names_to = "time_order", values_to = "time_value")

smp.ta <- soil.result.ta %>% 
  left_join(frmlnd.ward.soil, by = "plotid") %>% 
  left_join(prod.grn.sp.17, by = "plotid") %>% 
  mutate(prod_grn_sp = ifelse(is.na(prod_grn_sp), "no", prod_grn_sp))

# Analysis ----
## Description ----
# show overall pattern  
ggplot(smp.ha.long) + 
  geom_boxplot(aes(soil, time_value, color = surface)) + 
  facet_grid(ward ~ prod_grn_sp)
# bug: 姑且不论地块内部的差异，将同一地块同一层的数据合并起来

## Single factor ----
### Dry land ----
# Differ variables 
ggplot(smp.ha.long) + geom_boxplot(aes(surface, time_value))
ggplot(smp.ha.long) + geom_boxplot(aes(ward, time_value)) + 
  scale_x_discrete(drop = FALSE)
ggplot(smp.ha.long) + geom_boxplot(aes(ward_agg, time_value)) + 
  scale_x_discrete(drop = FALSE)
ggplot(smp.ha.long) + geom_boxplot(aes(soil, time_value)) + 
  theme(axis.text.x = element_text(angle = 90))
ggplot(smp.ha.long) + geom_boxplot(aes(prod_grn_sp, time_value))

# empty list to store statistical analysis results
sngl.sta.res <- numeric()
for (i in c("surface", "ward", "ward_agg", "soil", "prod_grn_sp")) {
  sngl.sta.res <- 
    c(sngl.sta.res, 
      kruskal.test(smp.ha.long[["time_value"]] ~ smp.ha.long[[i]]) %>% 
        .$p.value)
}
sngl.sta.res <- tibble(
  group = c("surface", "ward", "ward_agg", "soil", "prod_grn_sp"), 
  p_value = sngl.sta.res
) %>% 
  mutate(p_mark = case_when(
    p_value < 0.001 ~ "***", 
    p_value < 0.01 ~ "**", 
    p_value < 0.05 ~ "*"
  ))
sngl.sta.res
# 基本结论：
# 不同层、行政区、是否生产绿地之间有显著差异，但不同土壤之间无显著差异

### Paddy field ----
ggplot(smp.ta) + geom_boxplot(aes(ward, mean_depth_cm))
ggplot(smp.ta) + geom_boxplot(aes(soil, mean_depth_cm)) + 
  theme(axis.text.x = element_text(angle = 90))
ggplot(smp.ta) + geom_boxplot(aes(prod_grn_sp, mean_depth_cm))

## Two factor ----
# bug: 如何进行统计分析？
ggplot(smp.ha.long) + 
  geom_boxplot(aes(surface, time_value)) + 
  facet_wrap(.~ ward, scales = "free_y")
ggplot(smp.ha.long) + 
  geom_boxplot(aes(surface, time_value)) + 
  facet_wrap(.~ soil, scales = "free_y")
ggplot(smp.ha.long) + 
  geom_boxplot(aes(surface, time_value)) + 
  facet_wrap(.~ prod_grn_sp, scales = "free_y")

# Export ----
# 导出王闻报告所需旱地单因素对比图片
png("RProcData/旱地土壤分层对比.png", width = 1000, height = 1000, res = 300)
ggplot(smp.ha.long) + geom_boxplot(aes(surface, time_value)) + 
  theme(text = element_text(size = 10)) + 
  labs(x = "Surface layer", y = "Time (s)")
dev.off()

png("RProcData/旱地各区对比.png", width = 1000, height = 1000, res = 300)
ggplot(smp.ha.long) + geom_boxplot(aes(ward, time_value)) + 
  scale_x_discrete(drop = FALSE) + 
  theme(
    axis.text.x = element_text(angle = 90), text = element_text(size = 9)) + 
  labs(x = "Ward", y = "Time (s)")
dev.off()

png("RProcData/旱地土壤类型对比.png", width = 1000, height = 1000, res = 300)
ggplot(smp.ha.long) + geom_boxplot(aes(soil, time_value)) + 
  theme(
    axis.text.x = element_text(angle = 90), text = element_text(size = 9)) + 
  labs(x = "Soil type", y = "Time (s)")
dev.off()
