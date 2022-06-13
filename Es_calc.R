# Statement ----

# Package ----
library(openxlsx)
library(readxl)
library(shapefiles)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(showtext)
showtext_auto()
# 漏洞：需要提前读入土壤样品分析的结果

# Function ----
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
    # 计算稻米和蔬菜的固碳量并加和，计算方法为：
    # 吸收二氧化碳量=干物质生物量*1.63
    # 其中：干物质生物量=产量*总重量和产量之比*(1-含水量)
    # 结果单位为：吨二氧化碳
    mutate(cseq.rice = rice * (1 + 1.4) * (1 - 0.1) * 1.63, 
           cseq.veg = veg * (1 + 0.5) * (1 - 0.8) * 1.63) %>% 
    mutate(cseq = cseq.veg + cseq.rice) %>% 
    select(plotid, type, ward, area, rice, veg, cseq)
  
  # 返回结果
  return(prod.cseq)
}

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

# 函数：计算固氮量
# 参数：
# tot.ward.ha.area：全市口径各区旱地面积
# ward.ha.area：城市农业口径各区旱地面积
GetNfix <- function(tot.ward.ha.area, ward.ha.area) {
  ward.nfix <- tot.ward.ha.area[c("ward", "tot_ha_area")] %>% 
    left_join(ward.ha.area, by = "ward") %>% 
    # 计算城市农业旱地和总体旱地面积的比例
    mutate(rate = area / tot_ha_area) %>% 
    left_join(tot.ward.nfix, by = "ward") %>% 
    # 推算生产绿地的固氮量
    mutate(nfix = nfix * rate) %>% 
    # 替换NaN值
    mutate(nfix = ifelse(is.na(nfix), 0, nfix)) %>% 
    select(ward, nfix)
  
  return(ward.nfix)
}

# 函数：计算各地块降温效应
# 参数：
# frmlnd.area：各地块面积数据
# bug：需要将降温得分转化成温差值
GetCool <- function(frmlnd.area) {
  # 计算思路：基于《Zardo, L., Geneletti, D., Pérez-Soba, M., Van Eupen, M., 2017. Estimating the cooling capacity of green infrastructures to support urban planning. Ecosystem Services 26, 225–235.》中Fig.2和Fig.3中“Continental region”的部分进行赋分。首先判断得分，假设水田和“water”相当，而旱地和“grass”相当，对不同面积的水田和旱地赋予不同得分，其中：对于水田，小于2公顷得20分，否则得75分；对于旱地，小于2公顷得19分，否则得69分。其次，根据Fig.3获得得分对应的最大降温效应，0-20分为1摄氏度，61-80为3.8摄氏度。最后，将该降温效应乘以对应的地块面积，得到本研究的降温效应。
  # 漏洞： 是用降温度数乘以面积，还是直接采用Fig2的得分乘以面积。因为如果用降温度数的话，那水田和旱地就没区别了：不管是水田还是旱地，小于2公顷都是降温1度，大于2公顷都是降温3.8度。

  # 计算水田降温效应
  frmlnd.cool.ta <- frmlnd.area %>% 
    # 提取水田部分地块
    subset(type == "ta") %>% 
    # 加入降温效应得分
    mutate(cool_score = ifelse(area < 20000, 1, 3.8)) %>% 
    mutate(cool = cool_score * area)
  
  # 计算旱地降温效应
  frmlnd.cool.ha <- frmlnd.area %>% 
    # 提取水田部分地块
    subset(type == "ha") %>% 
    # 加入降温效应得分
    mutate(cool_score = ifelse(area < 20000, 1, 3.8)) %>% 
    mutate(cool = cool_score * area)
  
  # 合并水田和旱地的结果
  frmlnd.cool <- rbind(frmlnd.cool.ta, frmlnd.cool.ha) %>% 
    select(plotid, ward, cool)
  
  return(frmlnd.cool)
}

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

# function: min-max scaling
# parameters: 
# x: numeric vector
ScaleMinMax <- function(x) {
  output <- (x - min(x)) / (max(x) - min(x))
  return(output)
}

# Get data ----
# 京都市各区
kWard <- c("右京区", "西京区", "北区", "上京区", "中京区", "下京区", "南区",  
           "左京区", "東山区", "伏見区", "山科区")

# 生态系统服务项目
kEcoSvs <- c("rice", "veg", "cseq", "nfix", "cool", "ha_flood", "ta_flood")

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
frmlnd.area.07 <- read.shapefile("GProcData/Frmlnd_2007_add_ward") %>%
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  rename_with(tolower) %>% 
  rename(plotid = id) %>% 
  select(plotid, type, ward, area)
frmlnd.area.17 <- read.shapefile("GProcData/Frmlnd_2017_add_ward") %>%
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  rename_with(tolower) %>% 
  rename(plotid = id) %>% 
  select(plotid, type, ward, area)

# 计算全市旱地面积：结果单位为平方米
tot.ward.ha.area.07 <- 
  # 读取2007年的全市旱地面积，单位为公顷
  GetTotHa("RRawData/Kyoto_all_farmland_area_2007.xls") %>% 
  # 将单位转化为平方米
  mutate(tot_ha_area = tot_ha_area * 10000)
tot.ward.ha.area.17 <- 
  # 读取2017年的全市旱地面积，单位为100平方米
  GetTotHa("RRawData/Kyoto_all_farmland_area_2017.xlsx") %>% 
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

# 汇总计算2007年和2017年各区城市农业水田面积
ward.ta.area.07 <- frmlnd.area.07 %>% 
  subset(type == "ta") %>%
  group_by(ward) %>% 
  summarise(area = sum(area))
ward.ta.area.17 <- frmlnd.area.17 %>% 
  subset(type == "ta") %>%
  group_by(ward) %>% 
  summarise(area = sum(area))

# Analysis ----
## Production and carbon seq ----
# 计算2007年和2017年的生产服务和固碳服务
frmlnd.prod.cseq.07 <- 
  GetProdCseq(prodeff.07, frmlnd.area.07)
frmlnd.prod.cseq.17 <- 
  GetProdCseq(prodeff.17, frmlnd.area.17)

# 汇总计算区级生产服务和固碳服务
ward.prod.cseq.07 <- frmlnd.prod.cseq.07 %>% 
  group_by(ward) %>% 
  summarise(rice = sum(rice), veg = sum(veg), cseq = sum(cseq)) %>% 
  ungroup()
ward.prod.cseq.17 <- frmlnd.prod.cseq.17 %>% 
  group_by(ward) %>% 
  summarise(rice = sum(rice), veg = sum(veg), cseq = sum(cseq)) %>% 
  ungroup()

## N fix ----
# 思路：
# 将各区豆类产量，乘以生产绿地旱地和农地旱地总面积的比例，推算出各区生产绿地的
# 豆类产量；再结合文献中的豆科植物单位产量固氮量计算总固氮量

# 计算全市固氮量
tot.ward.nfix <- 
  # 读取豆科作物产量
  read_xlsx("RRawData/Legume_area_production.xlsx", sheet = "LegumeData") %>% 
  rename(yield = yield_t) %>% 
  select(species, ward, area_ha, yield) %>% 
  # 基于文献提取各种豆科植物单位面积固氮量
  # 采用Herridge et al. 2008文献中
  # Table 5的Calculated from Table 4 (kg N/ha/year)列的数据
  # 各类豆科作物的日文-英文对应关系为：
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
  # 替换旱地面积和产量为NA的值
  mutate(
    area_ha = ifelse(area_ha == "-", 0, area_ha), 
    yield = ifelse(is.na(yield), 0, yield)) %>% 
  mutate(area_ha = as.numeric(area_ha)) %>% 
  # 计算固氮量，结果单位为：千克氮每年
  mutate(nfix = nfix_rate * area_ha) %>% 
  # 汇总计算各区总固氮量，结果单位为：千克氮每年
  group_by(ward) %>% 
  summarise(nfix = sum(nfix)) %>% 
  ungroup()

# 推算生产绿地的固氮量，单位为：千克氮每年
ward.nfix.07 <- GetNfix(tot.ward.ha.area.07, ward.ha.area.07)
ward.nfix.17 <- GetNfix(tot.ward.ha.area.17, ward.ha.area.17)

## Cooling effect ---- 
# 计算各地块降温效应
frmlnd.cool.07 <- GetCool(frmlnd.area = frmlnd.area.07)
frmlnd.cool.17 <- GetCool(frmlnd.area = frmlnd.area.17)

# 汇总计算各区降温效应
ward.cool.07 <- frmlnd.cool.07 %>% 
  group_by(ward) %>% 
  summarise(cool = sum(cool)) %>% 
  ungroup()
ward.cool.17 <- frmlnd.cool.17 %>% 
  group_by(ward) %>% 
  summarise(cool = sum(cool)) %>% 
  ungroup()

## Flood mitigation ----
# 计算思路：对旱地和水田分别评价，其中：旱地采用透水率，水田采用储水量。旱地部分，基于土壤调查结果，计算各区土壤样品透水率平均值，将该值赋予对应行政区的旱地土壤，然后用该土壤透水率和对应旱地面积乘积作为旱地洪水风险防范的得分；水田部分，同理，将各区水田深度抽样调查数据的平均值赋予各区水田，然后乘以对应的面积得到储水量。
# 漏洞：旱地透水率的空间化不尽合理，是否考虑采用插值方法？且透水率乘以对应旱地面积的方式也不尽合理

# 汇总计算2007年和2017年各区城市农业旱地透水率
ward.ha.floodeff <- smp.ha %>% select(ward, time_1, time_2, time_3) %>% 
  pivot_longer(cols = c(time_1, time_2, time_3), 
               names_to = "time_id", values_to = "time") %>% 
  group_by(ward) %>% 
  summarise(time = mean(time)) %>% 
  ungroup() %>% 
  # 补全缺失的行政区的数值
  rbind(., data.frame(
    ward = c("上京区", "中京区", "東山区"), 
    time = .$time[which(.$ward == "下京区")]))

# 计算各区旱地洪水缓解效应
ward.ha.flood.07 <- ward.ha.area.07 %>% 
  left_join(ward.ha.floodeff, by = "ward") %>% 
  mutate(ha_flood = time * area)

ward.ha.flood.17 <- ward.ha.area.17 %>% 
  left_join(ward.ha.floodeff, by = "ward") %>% 
  mutate(ha_flood = time * area)

# 汇总计算各区水田样本的深度平均值
ward.ta.floodeff <- smp.ta %>% select(ward, depth_1, depth_2, depth_3) %>% 
  pivot_longer(cols = c(depth_1, depth_2, depth_3), 
               names_to = "depth_id", values_to = "depth") %>% 
  group_by(ward) %>% 
  summarise(depth = mean(depth)) %>% 
  ungroup() %>% 
  # 漏洞：补全缺失的行政区的数值
  rbind(., data.frame(
    ward = c("北区", "上京区", "中京区", "東山区", "下京区"), 
    depth = c(.$depth[which(.$ward == "左京区")], 
              rep(.$depth[which(.$ward == "南区")], 4))))

# 计算各区水田洪水缓解效应
ward.ta.flood.07 <- ward.ta.area.07 %>% 
  left_join(ward.ta.floodeff, by = "ward") %>% 
  mutate(ta_flood = depth * area)

ward.ta.flood.17 <- ward.ta.area.17 %>% 
  left_join(ward.ta.floodeff, by = "ward") %>% 
  mutate(ta_flood = depth * area)

## Sum to ward level ----
# 漏洞：存在NA值，需要确认是否应为0
ward.es.07 <- Reduce(
  function(x, y) {left_join(x, y, by = "ward")}, 
  list(tibble(ward = kWard), 
       ward.prod.cseq.07, 
       ward.nfix.07, 
       ward.cool.07, 
       select(ward.ha.flood.07, ward, ha_flood), 
       select(ward.ta.flood.07, ward, ta_flood))
)
ward.es.07[is.na(ward.es.07)] <- 0
ward.es.17 <- Reduce(
  function(x, y) {left_join(x, y, by = "ward")}, 
  list(tibble(ward = kWard), 
       ward.prod.cseq.17, 
       ward.nfix.17, 
       ward.cool.17, 
       select(ward.ha.flood.17, ward, ha_flood), 
       select(ward.ta.flood.17, ward, ta_flood))
)
ward.es.17[is.na(ward.es.17)] <- 0

# Export MS Excel ----
# 导出各区生态系统服务
WriteCamelCsv(ward.es = ward.es.07, file.name = "RProcData/Ward_es_2007.csv")
WriteCamelCsv(ward.es = ward.es.17, file.name = "RProcData/Ward_es_2017.csv")

# Visualization ----
# 分析各区ES差异
# 区级生态系统服务结果标准化
ward.es.07.scale <- data.frame(ward = ward.es.07$ward) %>% 
  cbind(apply(select(ward.es.07, -ward), 2, ScaleMinMax))
ward.es.17.scale <- data.frame(ward = ward.es.17$ward) %>% 
  cbind(apply(select(ward.es.17, -ward), 2, ScaleMinMax))

# 输出图片
png(filename = "RProcData/Es_ward.png", width = 1200, height = 2400, res = 200)
(pivot_longer(ward.es.07.scale, cols = -ward, names_to = "es") %>% 
  mutate(year = "2007") %>% 
  rbind(pivot_longer(ward.es.17.scale, cols = -ward, names_to = "es") %>% 
          mutate(year = "2017")) %>% 
  subset(es != "nfix") %>%  # 漏洞：暂时去除固氮服务
  mutate(es = factor(es, levels = kEcoSvs), 
         ward = factor(ward, levels = kWard)) %>% 
  ggplot() + 
  geom_col(aes(x = ward, y = value, fill = year), position = "dodge") + 
  facet_wrap(.~ es, ncol = 1))
dev.off()
