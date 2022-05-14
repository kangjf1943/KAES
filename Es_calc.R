library(openxlsx)
library(readxl)
library(shapefiles)
library(dplyr)
library(ggplot2)
library(patchwork)
library(showtext)
showtext_auto()

# Production and carbon sequestration ----
# 读取各区单位面积产量
prodeff <- 
  read.xlsx("RRawData/Rich_veg_production_per_area_by_ward.xlsx") %>% 
  as_tibble() %>% 
  # 将稻米产量单位由吨/10a转化为吨/平方米
  mutate(rice_07 = rice_07 / (10 * 100), 
         rice_17 = rice_17 / (10 * 100)) %>% 
  # 将蔬菜产量单位由千克/10a转化为吨/平方米
  mutate(veg_07 = (veg_07 / 1000) / (10 * 100), 
         veg_17 = (veg_17 / 1000) / (10 * 100))

# 提取2007年各区单位面积产量
prodeff.07 <- prodeff %>% 
  select(ward, rice_07, veg_07) %>% 
  rename(rice = rice_07, veg = veg_07)

# 提取2017年各区单位面积产量
prodeff.17 <- prodeff %>% 
  select(ward, rice_17, veg_17) %>% 
  rename(rice = rice_17, veg = veg_17)

# 读取各区各地块面积
frmlnd.area.07 <- read.shapefile("GProcData/Kyoto_prod_green_space_2007") %>%
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  rename_with(tolower) %>% 
  rename(ward = city_name) %>% 
  select(plotid, type, ward, area)

frmlnd.area.17 <- read.shapefile("GProcData/Kyoto_prod_green_space_2017") %>%
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  rename_with(tolower) %>% 
  rename(ward = city_name) %>%
  select(plotid, type, ward, area)

# 函数：计算各地块稻米和蔬菜产量
# 参数：
# x.prodeff：单位面积产量数据框
# x.area：各地块面积
GetProdCseq <- function(x.prodeff, x.area) {
  # 计算大米生产量
  riceprod <- 
    # 筛选出水田地块
    subset(x.area, type == "ta") %>% 
    # 加入单位面积产量数据
    left_join(x.prodeff, by = "ward") %>% 
    # 水田只生产稻米，故计算稻米产量，而蔬菜产量等于0
    mutate(rice = rice * area, veg = 0)
  
  # 计算蔬菜产量
  # 和上面类似地，筛选出旱地地块，计算蔬菜产量，而稻米产量等于0
  vegprod <- subset(x.area, type == "ha") %>% 
    left_join(x.prodeff, by = "ward") %>% 
    mutate(veg = veg * area, rice = 0) 
  
  # 合并稻米和蔬菜产量
  prod <- rbind(riceprod, vegprod)
  
  # 在此基础上计算固碳服务
  prod.cseq <- prod %>% 
    # 计算稻米和蔬菜的固碳量并加和
    mutate(cseq.rice = rice * (1 + 1.4) * (1 - 0.1), 
           cseq.veg = veg * (1 + 0.5) * (1 - 0.8)) %>% 
    mutate(cseq = cseq.veg + cseq.rice) %>% 
    select(plotid, type, ward, area, rice, veg, cseq)
  
  # 返回结果
  return(prod.cseq)
}

# 计算2007年和2017年的生产服务和固碳服务
frmlnd.prod.cseq.07 <- 
  GetProdCseq(prodeff.07, frmlnd.area.07)
frmlnd.prod.cseq.17 <- 
  GetProdCseq(prodeff.17, frmlnd.area.17)

# N fix ----
# 思路：
# 将各区豆类产量，乘以生产绿地旱地和农地旱地总面积的比例，推算出各区生产绿地的
# 豆类产量；再结合文献中的豆科植物单位产量固氮量计算总固氮量

# 读取整理各区农地总面积
# 函数：读取整合各区农地总面积
# 参数：
# x：原始数据路径及文件名
GetTotHa <- function(x) {
  frmlnd.type.area <- read_excel(x)[c(10:20), c(2:8)] %>%  
    # 重命名各列列名
    rename_with(~ c("ward", "total", "self_support_type", "sale_type", 
                  "sale_paddy_field", "sale_dry_land", "sale_orchard")) %>% 
    # 替换非数字数值
    apply(2, function(x) {gsub("Ｘ", 0, x)}) %>% 
    apply(2, function(x) {gsub("－", 0, x)}) %>%
    apply(2, function(x) {gsub("X", 0, x)}) %>%
    apply(2, function(x) {gsub("-", 0, x)}) %>% 
    as_tibble()
  
  # 将数据类型均转换为数字类型
  frmlnd.type.area[2:7] <- apply(frmlnd.type.area[2:7], 2, as.numeric) %>% as_tibble()
  
  frmlnd.type.area <- frmlnd.type.area %>% 
    # 重构总面积为自给型和贩卖型之和
    mutate(total = self_support_type + sale_type) %>% 
    # 各区旱地面积计算：
    # 假设总面积中旱地占比和贩卖型农田中旱地占比相同
    mutate(tot_ha_area = total * sale_dry_land / sale_type) %>% 
    select(ward, tot_ha_area)
  
  # 替换计算结果中的NaN
  frmlnd.type.area[is.na(frmlnd.type.area)] <- 0
  
  # 返回结果
  return(frmlnd.type.area)
}

# 计算全市旱地面积
# 计算2007年的全市旱地面积，单位为公顷
tot.ward.ha.area.07 <- GetTotHa("RRawData/Kyoto_all_farmland_area_2007.xls") %>% 
  # 将单位转化为平方米
  mutate(tot_ha_area = tot_ha_area * 10000)

# 计算2017年的全市旱地面积，单位为100平方米
tot.ward.ha.area.17 <- GetTotHa("RRawData/Kyoto_all_farmland_area_2017.xlsx") %>% 
  # 将单位转化为平方米
  mutate(tot_ha_area = tot_ha_area * 100)

# 汇总计算2007年和2017年各区生产绿地旱地面积
ward.ha.area.07 <- frmlnd.area.07 %>% 
  subset(type == "ha") %>%
  group_by(ward) %>% 
  summarise(area = sum(area))

ward.ha.area.17 <- frmlnd.area.17 %>% 
  subset(type == "ha") %>%
  group_by(ward) %>% 
  summarise(area = sum(area))

# 计算全市固氮量
tot.ward.nfix <- 
  # 读取豆科作物产量
  read_xlsx("RRawData/Legume_area_production.xlsx", sheet = "LegumeData") %>% 
  rename(yield = yield_t) %>% 
  select(species, ward, area_ha, yield) %>% 
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
  mutate(area_ha = as.numeric(gsub("-", 0, area_ha))) %>% 
  # 替换旱地面积和产量为NA的值
  mutate(area_ha = ifelse(is.na(area_ha), 0, area_ha), 
         yield = ifelse(is.na(yield), 0, yield)) %>% 
  # 计算固氮量，单位为：千克N
  mutate(nfix = nfix_rate * area_ha) %>% 
  # 汇总计算各区总量
  group_by(ward) %>% 
  summarise(nfix = sum(nfix)) %>% 
  ungroup()

# 推算生产绿地固氮量
# 函数：计算固氮量
# 参数：
# tot.ward.ha.area：全市口径各区旱地面积
# ward.ha.area：生产绿地口径各区旱地面积
GetNFix <- function(tot.ward.ha.area, ward.ha.area) {
  ward.nfix <- tot.ward.ha.area[c("ward", "tot_ha_area")] %>% 
    left_join(ward.ha.area, by = "ward") %>% 
    # 计算生产绿地旱地和总体旱地面积的比例
    mutate(rate = area / tot_ha_area) %>% 
    left_join(tot.ward.nfix, by = "ward") %>% 
    # 推算生产绿地的固氮量
    mutate(nfix = nfix * rate) %>% 
    # 替换NaN值
    mutate(nfix = ifelse(is.na(nfix), 0, nfix)) %>% 
    select(ward, nfix)
  
  return(ward.nfix)
}

# 并且推算生产绿地的固氮量
ward.nfix.07 <- GetNFix(tot.ward.ha.area.07, ward.ha.area.07)
ward.nfix.17 <- GetNFix(tot.ward.ha.area.17, ward.ha.area.17)

# Cooling effect ---- 
frmlnd.area = frmlnd.area.07

# 函数：计算各地块降温效应
# 参数：
# frmlnd.area：各地块面积数据
GetCool <- function(frmlnd.area) {
  # 计算水田降温效应
  frmlnd.cool.ta <- frmlnd.area %>% 
    # 提取水田部分地块
    subset(type == "ta") %>% 
    # 加入降温效应得分
    mutate(cool_score = ifelse(area < 20000, 20, 75)) %>% 
    mutate(cool = cool_score * area)
  
  # 计算旱地降温效应
  frmlnd.cool.ha <- frmlnd.area %>% 
    # 提取水田部分地块
    subset(type == "ha") %>% 
    # 加入降温效应得分
    mutate(cool_score = ifelse(area < 20000, 19, 69)) %>% 
    mutate(cool = cool_score * area)
  
  # 合并水田和旱地的结果
  frmlnd.cool <- rbind(frmlnd.cool.ta, frmlnd.cool.ha) %>% 
    select(plotid, cool)
  
  return(frmlnd.cool)
}

frmlnd.cool.07 <- GetCool(frmlnd.area = frmlnd.area.07)
frmlnd.cool.17 <- GetCool(frmlnd.area = frmlnd.area.17)

# Sum to ward level ----
# 计算各区生态系统服务量
SumEs <- function(frmlnd.prod.cseq, frmlnd.cool, ward.nfix) {
  es.ward <- frmlnd.prod.cseq %>% 
    select(plotid, ward, rice, veg, cseq) %>% 
    left_join(frmlnd.cool %>% select(plotid, cool), by = "plotid") %>% 
    group_by(ward) %>% 
    summarise( 
      rice = sum(rice), 
      veg = sum(veg), 
      cseq = sum(cseq), 
      cool = sum(cool)) %>% 
    ungroup() %>% 
    # 合并固氮量结果
    left_join(ward.nfix[c("ward", "nfix")], by = "ward")
  
  return(es.ward)
}

ward.es.07 <- SumEs(frmlnd.prod.cseq.07, frmlnd.cool.07, ward.nfix.07)
ward.es.17 <- SumEs(frmlnd.prod.cseq.17, frmlnd.cool.17, ward.nfix.17)

# Export MS Excel ----
# 函数：将通过下划线连接的字符串改成驼峰式写法
# 参数：
# x：字符串
TurnCamel <- function(x) {
  # 根据下划线拆分字符串
  strsplit(x, "_")[[1]] %>% 
    # 将每个元素都改成首字母大写
    Hmisc::capitalize() %>% 
    # 将新的字符串拼接起来
    Reduce(paste0, .) %>% 
    return()
}

# 函数：将数据框列名重命名为驼峰式写法，再输出为*.csv文件
# 参数：
# ward.es：待写出的数据框
# file.name：输出路径和文件名
WriteCamelCsv <- function(ward.es, file.name) {
  for (i in names(ward.es)) {
    names(ward.es)[names(ward.es) == i] <- TurnCamel(i)
  }
  write.csv(ward.es, file.name)
}

# 导出各地块生态系统服务
# 注意，固氮服务只有区粒度的结果，而无地块粒度的结果，因为不知道哪些地块种的是豆
# 科植物，因此无法将结果分配到地块
frmlnd.prod.cseq.07 %>% left_join(frmlnd.cool.07, by = "plotid") %>% 
  WriteCamelCsv(file.name = "GProcData/Frmlnd_es_2007.csv")
frmlnd.prod.cseq.17 %>% left_join(frmlnd.cool.17, by = "plotid") %>% 
  WriteCamelCsv(file.name = "GProcData/Frmlnd_es_2017.csv")

# 导出各区生态系统服务
WriteCamelCsv(ward.es = ward.es.07, file.name = "GProcData/Ward_es_2007.csv")
WriteCamelCsv(ward.es = ward.es.17, file.name = "GProcData/Ward_es_2017.csv")

# Visualization ----
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

