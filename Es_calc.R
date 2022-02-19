library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)
library(showtext)
showtext_auto()

es_frmlnd <- 
  read.xlsx("/Users/Kang/Documents/R/KAES/GIS/ProcData/Production_Cui.xlsx") %>%
  select(id, CITY_NAME_, type07, type2017, 
         `yasai7.(kg)`, `yasai17.(kg)`, `rice7.(t)`, `rice17.(t)`) %>% 
  rename(ward = CITY_NAME_, type_07 = type07, type_17 = type2017, 
         yasai_07 = `yasai7.(kg)`, yasai_17 = `yasai17.(kg)`, 
         rice_07 = `rice7.(t)`, rice_17 = `rice17.(t)`) %>% 
  # 将蔬菜产量单位转换为吨
  mutate(yasai_07 = yasai_07 / 1000, yasai_17 = yasai_17 / 1000) %>%
  mutate(cseq_yasai_07 = yasai_07 * (1 + 0.5) * (1 - 0.8), 
         cseq_yasai_17 = yasai_17 * (1 + 0.5) * (1 - 0.8), 
         cseq_rice_07 = rice_07 * (1 + 1.4) * (1 - 0.1), 
         cseq_rice_17 = rice_17 * (1 + 1.4) * (1 - 0.1)) %>% 
  mutate(cseq_07 = cseq_yasai_07 + cseq_rice_07, 
         cseq_17 = cseq_yasai_17 + cseq_rice_17)

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
  ungroup()
write.xlsx(es_ward, "/Users/Kang/Documents/R/KAES/GIS/ProcData/R ES_ward.xlsx")

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
  (plot_es_ward[[13]] + plot_es_ward[[14]] + plot_es_ward[[15]]) 
dev.off()

