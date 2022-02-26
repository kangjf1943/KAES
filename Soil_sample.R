library(openxlsx)
library(dplyr)
library(ggplot2)
library(showtext)
showtext_auto()

# 旱地采样设计 ----
# 采样设计需要考虑土壤类型和行政区
frmlnd <- read.xlsx("RRawData/GIS 京都農地_soil.xlsx") %>% 
  rename(soil = `土壌大�`) %>% 
  subset(type2017 == "ha")
# 先去除id重复的项目
dupid <- frmlnd$id[which(duplicated(frmlnd$id) == TRUE)]
frmlnd <- frmlnd[which(!frmlnd$id %in% dupid), ]
# 先看土壤和行政区的组合有多少种
unique(frmlnd[c("CITY_NAME_", "soil")]) %>% nrow()

# 类型过多，合并部分行政区
frmlnd$ward_agg <- frmlnd$CITY_NAME_
frmlnd$ward_agg[frmlnd$CITY_NAME_ %in% 
                  c("南区", "下京区", "中京区", "上京区", "東山区")] <- "中部"

# 采样设计需要满足：各种土壤类型至少有3个样地，各区至少有3个样地

# 看各类土壤类型下有多少个区
frmlnd_soil <- frmlnd %>% 
  group_by(soil, ward_agg) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  as.data.frame()
frmlnd_soil

# グライ土仅覆盖2个区，因此在伏见区多取一个样地
tarsmp_ha <- frmlnd_soil[c(1, 2), c(1, 2)]
tarsmp_ha$num <- c(2, 1)

# 未定義土壤覆盖3个区，因此全部入选
tarsmp_ha <- rbind(
  tarsmp_ha, tibble(frmlnd_soil[c(3:5), c(1, 2)], num = rep(1, 3)))

# 赤黄色土仅覆盖1个区，因此在本区选3个样地
tarsmp_ha <- rbind(
  tarsmp_ha, tibble(frmlnd_soil[23, c(1, 2)], num = 3))

# 查看全部统计中各区的分布
table(frmlnd_soil$ward_agg)

# 中部仅有3个，因此中部灰色低地土和褐色低地土的全部入选
# 右京区仅有2个，因此右京区未熟土和褐色低地土的入选，而且要加选一个右京区：褐色低地土样地较多，因此加选该类型
# 山科区仅有2个，因此山科区未熟土和灰色低地土入选，而且要加选一个山科区：两个土壤类型数量相近，之后根据土壤类型分布加选
tarsmp_ha <- rbind(
  tarsmp_ha, 
  tibble(frmlnd_soil[c(12, 18), c(1, 2)], num = c(1, 1)), 
  tibble(frmlnd_soil[c(8, 20), c(1, 2)], num = c(1, 2)),
  tibble(frmlnd_soil[c(9, 15), c(1, 2)], num = c(1, 1))
)

# 查看不足的土壤和区的候选
frmlnd_soil[which(
  frmlnd_soil$ward_agg %in% c("伏見区", "北区", "山科区", "左京区") & 
    frmlnd_soil$soil %in% c("未熟土", "灰色低地土")), ]

# 山科区不足1个样地，加选其未熟土
# 除了山科区，未熟土和灰色低地土包含的区都是伏见区、北区、左京区，因此任选伏见区1个为灰色低地土，左京区2个，北区2个
tarsmp_ha <- rbind(
  tarsmp_ha, tibble(frmlnd_soil[9, c(1, 2)], num = 1))
tarsmp_ha <- rbind(
  tarsmp_ha, 
  tibble(frmlnd_soil[13, c(1, 2)], num = 1), 
  tibble(frmlnd_soil[c(10, 16), c(1, 2)], num = c(1, 1)), 
  tibble(frmlnd_soil[c(7, 14), c(1, 2)], num = c(1, 1))
) %>% arrange(soil)

# 进一步整合方案，避免出现重复项
tarsmp_ha <- tarsmp_ha %>% 
  group_by(soil, ward_agg) %>% 
  summarise(num = sum(num)) %>% 
  ungroup()

# 查看结果分布
tarsmp_ha %>% 
  group_by(ward_agg) %>% 
  summarise(n = sum(num)) %>% 
  ungroup() %>% 
  ggplot() + geom_col(aes(ward_agg, n))
tarsmp_ha %>% 
  group_by(soil) %>% 
  summarise(n = sum(num)) %>% 
  ungroup() %>% 
  ggplot() + geom_col(aes(soil, n))

# 共有22个样地
sum(tarsmp_ha$num)
# 每个样地3个样点，2层土样，共需要132个土样

# 旱地样地选择 ----
# 函数：从规定土壤大类和行政区中随机取样
fun_sample <- function(name_soil, name_ward, n) {
  # 选取符合条件的子集
  frmlnd_sub <- frmlnd[which(
    frmlnd$soil == name_soil & 
      frmlnd$ward_agg == name_ward), ]
  # 提取符合条件的行数
  set.seed(1234)
  output <- sample(as.character(frmlnd_sub$id), n)
  return(output)
}

# 建立空向量以存储结果
tarsmp_ha_num <- character()

for (i in 1:nrow(tarsmp_ha)) {
  name_soil <- tarsmp_ha[[i, 1]]
  name_ward <- tarsmp_ha[[i, 2]]
  n <- tarsmp_ha[[i, 3]]
  tarsmp_ha_num <- c(tarsmp_ha_num, fun_sample(name_soil, name_ward, n))
}
tarsmp_ha_num <- data_frame(id = tarsmp_ha_num, sample = TRUE) %>% 
  mutate(id = as.numeric(id))
length(unique(tarsmp_ha_num$id))
write.xlsx(tarsmp_ha_num, "GProcData/R tarsmp_ha.xlsx")


# 水田样地选择 ----
ta_alt <- read.xlsx("RRawData/GIS Kyoto_soil_smpalt.xlsx") %>% 
  rename(buff_id = id_3)
# 去除重复项
dupid <- ta_alt$id[which(duplicated(ta_alt$id) == TRUE)]
ta_alt <- ta_alt[which(!ta_alt$id %in% dupid), ]

# 在每个缓冲区内选择一个样地
tarsmp_ta <- 
  ta_alt %>% 
  # 仅选择水田
  subset(type2017 == "ta") %>% 
  group_by(buff_id) %>%
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  mutate(sample = TRUE) %>% 
  select(id, sample, buff_id)
dim(tarsmp_ta)
# 有6块目标旱地周边没有水田
unique(ta_alt$buff_id[!ta_alt$buff_id %in% tarsmp_ta$buff_id])

# 导出目标水田的信息
write.xlsx(
  tarsmp_ta, "/Users/Kang/Documents/R/KAES/GProcData/R tarsmp_ta_near.xlsx")

# 手动在GIS上选择这4块旱地周边的水田