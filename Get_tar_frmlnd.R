# Packages ----
library(dplyr)

# 测试校正完的农田数据编号是否有重复
# 读取校正后的农田*.shp文件属性表
frmlnd.fix <- read.shapefile("GProcData/Kyoto_farmland_fix") %>% 
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  mutate(ChgYr = as.integer(as.character(ChgYr)), 
         PreChgYr = as.integer(as.character(PreChgYr)))

# 如果测试结果为True，就说明没有重复编号
sum(duplicated(frmlnd.fix$PlotId)) == 0

# 读取筛选后的农田*.shp文件属性表
frmlnd.filter <- read.shapefile("GProcData/Kyoto_farmland_filter") %>% 
  .$dbf %>% .$dbf %>% as_tibble() %>% 
  mutate(ChgYr = as.integer(as.character(ChgYr)), 
         PreChgYr = as.integer(as.character(PreChgYr)))

# 提取出2007年和2017年的目标地块
# 2007年的目标地块判断标准：类型为水田或旱地，且之后未发生变化
frmlnd.filter.2007 <- frmlnd.filter %>% 
  subset((Type == "ta" | Type == "ha") & Chg == 0) 
# 问题：这样筛选出来后的2007年图层，照理来说“AfterChg”列应全为空值，但实际上包含
# 了1个非空值的行
frmlnd.filter.2007[!is.na(frmlnd.filter.2007$AfterChg), ]
# 暂时先删除该行
frmlnd.filter.2007 <- frmlnd.filter.2007 %>% 
  subset(is.na(AfterChg))
# 问题：而且保留下来的数据中“ChgYr”的值各式各样，所以“ChgYr”列到底是什么意思呢？
# “Type”为“0”不是表示类型不变吗？
table(frmlnd.filter.2007$ChgYr)
# 加入新列，以表示该状态的对应年份
frmlnd.filter.2007 <- frmlnd.filter.2007 %>% 
  mutate(Yr = 2007)

# 2017年的目标地块判断标准：类型为水田或旱地，变化类型则包括三类，即未变化、发生
# 水田到旱地的转化或者反向的转化、新增的农田
frmlnd.filter.2017 <- frmlnd.filter %>% 
  subset((Type == "ta" | Type == "ha") & (Chg == 0 | Chg == 2 | Chg == 3)) 
# 问题：这样筛选出来后的2017年图层，照理来说“AfterChg”列应全为空值，但实际上包含
# 了2个非空值的行
frmlnd.filter.2017[!is.na(frmlnd.filter.2017$AfterChg), ]
# 姑且忽略“AfteChg”为“9058”的行，删除值为“道路”的行
frmlnd.filter.2017 <- frmlnd.filter.2017 %>% 
  subset(is.na(AfterChg)) %>% 
  # 加入新列，以表示该状态的对应年份
  mutate(Yr = 2017)

# 将筛选后的两个属性表放回GIS文件夹，之后通过Join attributes by field value生成
# 2007年和2017年的目标地块多边形图层
write.csv(frmlnd.filter.2007, "GProcData/Kyoto_farmland_2007.csv")
write.csv(frmlnd.filter.2017, "GProcData/Kyoto_farmland_2017.csv")
