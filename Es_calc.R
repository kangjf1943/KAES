library(openxlsx)
library(shapefiles)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(showtext)
showtext_auto()

# Production and carbon seq ----
# 读取各区单位面积产量
frmlnd.prodeff <- 
  read.xlsx("RRawData/Rich_veg_production_per_area_by_ward.xlsx") %>% 
  as_tibble() %>% 
  # 将稻米产量单位由t/10a转化为t/平方米
  mutate(rice_07 = rice_07 / (10 * 100), 
         rice_17 = rice_17 / (10 * 100)) %>% 
  # 将蔬菜产量单位由kg/10a转化为t/平方米
  mutate(veg_07 = (veg_07 / 1000) / (10 * 100), 
         veg_17 = (veg_17 / 1000) / (10 * 100))
frmlnd.prodeff.07 <- frmlnd.prodeff %>% 
  select(ward, rice_07, veg_07) %>% 
  rename(rice = rice_07, veg = veg_07)
frmlnd.prodeff.17 <- frmlnd.prodeff %>% 
  select(ward, rice_17, veg_17) %>% 
  rename(rice = rice_17, veg = veg_17)

# 读取各区各地块面积
frmlnd.area.07 <- read.shapefile("GProcData/Kyoto_prod_green_space_2007") %>%
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  select(PlotId, Type, CITY_NAME, Area) %>% 
  rename_with(tolower)

frmlnd.area.17 <- read.shapefile("GProcData/Kyoto_prod_green_space_2017") %>%
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  select(PlotId, Type, CITY_NAME, Area) %>% 
  rename_with(tolower)

# 函数：计算各地块稻米和蔬菜产量
GetProd <- function(x.prodeff, x.area) {
  # 计算大米生产量
  riceprod <- subset(x.area, type == "ta") %>% 
    left_join(x.prodeff, by = c("city_name" = "ward")) %>% 
    mutate(rice = rice * area) %>% 
    mutate(veg = 0)
  # 计算蔬菜产量
  vegprod <- subset(x.area, type == "ha") %>% 
    left_join(x.prodeff, by = c("city_name" = "ward")) %>% 
    mutate(veg = veg * area) %>% 
    mutate(rice = 0)
  # 合并稻米和蔬菜产量
  prod <- rbind(riceprod, vegprod)
  
  # 加入计算碳吸收
  prod.cseq <- prod %>% 
    mutate(cseq.veg = veg * (1 + 0.5) * (1 - 0.8), 
           cseq.rice = rice * (1 + 1.4) * (1 - 0.1)) %>% 
    mutate(cseq = cseq.veg + cseq.rice) %>% 
    select(plotid, type, city_name, area, rice, veg, cseq)
  
  return(prod.cseq)
}

es.frmlnd.prod.07 <- 
  GetProd(frmlnd.prodeff.07, frmlnd.area.07)
es.frmlnd.prod.17 <- 
  GetProd(frmlnd.prodeff.17, frmlnd.area.17)

# N fix ----
# 思路：
# 基于各区豆类产量，结合生产绿地旱地和农地旱地总面积的比例，推算各区生产绿地的
# 豆类产量；再结合文献中的豆科植物单位产量固氮量计算总固氮量

# 读取整理各区农地总面积
# 函数：读取整合各区农地总面积
# 输入：原始数据路径及文件名
fun_allfrmlnd <- function(x) {
  all_frmlnd <- read_excel(x) %>% 
    .[c(10:20), ] %>% 
    # 各区非果树总量按照总农地
    select(2:8)
  # 重命名各列列名
  names(all_frmlnd) <- 
    c("ward", "total", "self_support_type", 
      "sale_type", "sale_paddy_field", "sale_dry_land", "sale_orchard")
  all_frmlnd <- all_frmlnd %>% 
    apply(2, function(x) {gsub("Ｘ", 0, x)}) %>% 
    apply(2, function(x) {gsub("－", 0, x)}) %>%
    apply(2, function(x) {gsub("X", 0, x)}) %>%
    apply(2, function(x) {gsub("-", 0, x)}) %>%
    as_tibble() 
  # 将数据类型均转换为数字类型
  all_frmlnd[2:7] <- apply(all_frmlnd[2:7], 2, as.numeric) %>% as_tibble()
  
  all_frmlnd <- all_frmlnd %>% 
    # 重构总面积为自给型和贩卖型之和
    mutate(total = self_support_type + sale_type) %>% 
    # 各区旱地面积计算：
    # 假设总面积中旱地占比和贩卖型农田中旱地占比相同
    mutate(total_dry_land = total * sale_dry_land / sale_type)
  # 替换计算结果中的NaN
  all_frmlnd[is.na(all_frmlnd)] <- 0
  # 返回结果
  return(all_frmlnd)
}

# 读取并整理2007年的数据，单位为公顷
all_frmlnd_07 <- fun_allfrmlnd("RRawData/Kyoto_all_farmland_area_2007.xls")
# 将单位转化为平方米
all_frmlnd_07[2:8] <- apply(all_frmlnd_07[2:8], 2, function(x) {x*10000})

# 读取并整理2007年的数据，单位为公顷
all_frmlnd_17 <- fun_allfrmlnd("RRawData/Kyoto_all_farmland_area_2017.xlsx")
# 将单位转化为平方米
all_frmlnd_17[2:8] <- apply(all_frmlnd_17[2:8], 2, function(x) {x*100})
# 数据初步分析结果：
# 贩卖型为主；贩卖型中又以水田为主
# 大部分行政区2017年农地总面积相比2007年减少
# 问题：但是右京区反而增加了？

# 汇总计算2007年和2017年各区生产绿地旱地面积
es_dry_land_07 <- frmlnd.area.07 %>% 
  subset(type == "ha") %>%
  group_by(city_name) %>% 
  summarise(area = sum(area))

es_dry_land_17 <- frmlnd.area.17 %>% 
  subset(type == "ha") %>%
  group_by(city_name) %>% 
  summarise(area = sum(area))

# 读取豆科作物产量
all_legume_nfix <- 
  read_xlsx("RRawData/Legume_area_production.xlsx", sheet = "LegumeData") %>% 
  # 基于文献提取各种豆科植物单位面积固氮量
  # 采用Herridge et al. 2008文献中
  # Table 5的Calculated from Table 4 (kg N/ha/year)列的数据
  # 実えんどう = pea
  # さやえんどう = Other pulses
  # えだまめ = Other pulses
  # さやいんげん = common bean
  mutate(nfix_rate = case_when(
    species == "実えんどう" ~ 86, 
    species == "さやえんどう" ~ 41, 
    species == "えだまめ" ~ 41, 
    species == "さやいんげん" ~ 23
  )) %>% 
  # 整理并转化area_ha列数据类型
  mutate(area_ha = as.numeric(gsub("-", 0, area_ha)))
# 替换area_ha为NA的值
all_legume_nfix$area_ha[is.na(all_legume_nfix$area_ha)] <- 0
all_legume_nfix <- all_legume_nfix %>% 
  # 计算固氮量，单位为kg N
  mutate(nfix = nfix_rate * area_ha) %>% 
  # 汇总计算各区总量
  group_by(ward) %>% 
  summarise(nfix = sum(nfix)) %>% 
  ungroup()

# 合并豆科总固氮量和2007和2017年各区推算旱地总面积数据和生产绿地旱地的面积
# 并且推算生产绿地的固氮量
es_nfix <- 
  merge(all_frmlnd_07[c("ward", "total_dry_land")], 
        all_frmlnd_17[c("ward", "total_dry_land")], by = "ward") %>% 
  rename(total_dry_land_07 = total_dry_land.x, 
         total_dry_land_17 = total_dry_land.y) %>% 
  left_join(es_dry_land_07, by = "ward") %>% 
  left_join(es_dry_land_17, by = "ward") %>% 
  rename(es_dry_land_07 = "area.x", 
         es_dry_land_17 = "area.y") %>% 
  # 计算2007和2017年生产绿地旱地和总体旱地面积的比例
  mutate(rate_07 = es_dry_land_07 / total_dry_land_07, 
         rate_17 = es_dry_land_17 / total_dry_land_17) %>% 
  left_join(all_legume_nfix, by = "ward") %>% 
  # 计算2007年和2017年生产绿地的固氮量
  mutate(nfix_07 = nfix * rate_07, 
         nfix_17 = nfix * rate_17)
# 替换NaN值
es_nfix$nfix_17[is.nan(es_nfix$nfix_17)] <- 0

# 计算各区生态系统服务量
es_ward <- es_frmlnd %>% 
  group_by(ward) %>% 
  summarise(
    yasai_07 = sum(yasai_07), yasai_17 = sum(yasai_17), 
    yasai_chg = yasai_17 - yasai_07, 
    rice_07 = sum(rice_07), rice_17 = sum(rice_17), 
    rice_chg = rice_17 - rice_07, 
    cseq_yasai_07 = sum(cseq_yasai_07), cseq_yasai_17 = sum(cseq_yasai_17), 
    cseq_yasai_chg = cseq_yasai_17 - cseq_yasai_07, 
    cseq_rice_07 = sum(cseq_rice_07), cseq_rice_17 = sum(cseq_rice_17), 
    cseq_rice_chg = cseq_rice_17 - cseq_rice_07, 
    cseq_07 = sum(cseq_07), cseq_17 = sum(cseq_17), 
    cseq_chg = cseq_17 - cseq_07) %>% 
  ungroup() %>% 
  # 合并2007年和2017年固氮量结果
  left_join(es_nfix[c("ward", "nfix_07", "nfix_17")], by = "ward") %>%
  mutate(nfix_chg = nfix_17 - nfix_07)
write.xlsx(es_ward, "/Users/Kang/Documents/R/KAES/GProcData/R ES_ward.xlsx")

# 分析各区ES差异
plot_es_ward <- vector("list", length = ncol(es_ward) - 1)
names(plot_es_ward) <- names(es_ward)[-1]
for (i in names(plot_es_ward)) {
  plot_es_ward[[i]] <- ggplot(es_ward) + geom_col(aes_string(i, "ward"))
}
png(filename = "Es_ward.png", width = 1200, height = 2400, res = 200)
(plot_es_ward[[1]] + plot_es_ward[[2]] + plot_es_ward[[3]]) /
  (plot_es_ward[[4]] + plot_es_ward[[5]] + plot_es_ward[[6]]) /
  (plot_es_ward[[7]] + plot_es_ward[[8]] + plot_es_ward[[9]]) /
  (plot_es_ward[[10]] + plot_es_ward[[11]] + plot_es_ward[[12]]) /
  (plot_es_ward[[13]] + plot_es_ward[[14]] + plot_es_ward[[15]]) / 
  (plot_es_ward[[16]] + plot_es_ward[[17]] + plot_es_ward[[18]])
dev.off()

