library(tidyverse)
library(openxlsx)
library(readxl)
library(dplyr)
library(lubridate)

setwd("E:/Molecular Epidemiology/GLYCEMIA/GLY")
all <- read_excel("all.xlsx")
str(all)

####1.定位到治疗半年以上的数据####
#####1.1找到治疗半年以上的病载记录，去除半年以前的记录#####
TreatMore6ms <- all %>% group_by(ID) %>% arrange(ID, TestingDate) %>% 
  mutate(TimefromART.m = round(interval(start=ARTinitialDate, end=VLDate) / months(1), digits = 2)) %>% 
  mutate(ART6months = ifelse(!is.na(VL) & TimefromART.m > 6, cumsum(!is.na(VL) & TimefromART.m > 6),NA)) %>% 
  mutate(first_occurrence = if_else(any(ART6months == 1), which(ART6months == 1)[1], NA_integer_)) %>%
  filter(is.na(first_occurrence) | row_number() >= first_occurrence) %>% 
  select(-first_occurrence) %>% 
  filter(!all(is.na(ART6months))) %>% 
  mutate(ART6months = ifelse(is.na(ART6months), -1, ART6months))
#####1.2对治疗半年以上只有一次记录的ID，若不是VF则去除,因为两次记录才能判定LLV和VS#####
ids.1record <- TreatMore6ms %>% group_by(ID) %>% filter(!any(ART6months == 2))
ids.1record.removed <- ids.1record %>% group_by(ID) %>% filter(any(ART6months == 1 & VL< 1000))
TreatMore6ms <- anti_join(TreatMore6ms, ids.1record.removed, by = "ID")
write.xlsx(TreatMore6ms, "TreatMore6ms.xlsx")


####2.分VS,LLV,VF组####
#####2.1.提取出只有一条记录的ID，分组为VF#####
ids.1record.keep <- ids.1record %>% anti_join(ids.1record.removed, by = "ID")
ids.1record.keep <- ids.1record.keep %>% mutate(ART6months = ifelse(ART6months== -1, NA, ART6months))
ids.1record.keep <- ids.1record.keep %>% mutate(VLgroup = "VF")
#####2.2两条以上记录的，依据条件分VS,LLV,VF组#####
ids.more2record <- TreatMore6ms %>% anti_join(ids.1record.keep, by = "ID")
ids.2record <- ids.more2record %>% group_by(ID) %>% filter(ART6months %in% c(1, 2))
ids.2record <- ids.2record %>% group_by(ID) %>%
  mutate(
    VLgroup = case_when(
      all(VL <= 50) ~ "VS",
      first(VL) >= 1000 |
        (first(VL) < 1000 & lead(VL) >= 1000) |
        (first(VL) >= 200 & lead(VL) >= 200) ~ "VF",
      TRUE ~ NA_character_
    )) %>%
  group_by(ID) %>%
  mutate(VLgroup = max(VLgroup, na.rm = TRUE)) %>% 
  mutate(VLgroup = ifelse(is.na(VLgroup), "LLV", VLgroup))
#####2.3提取单个ID和对应的VL分组#####
ids.2record.select <- ids.2record %>% select(1, 11) %>% distinct(ID, .keep_all = TRUE)
ids.1record.keep.select <- ids.1record.keep %>% select(1, 11) %>% distinct(ID, .keep_all = TRUE)
ids.select.group <- bind_rows(ids.2record.select, ids.1record.keep.select)
#####2.4完成VL分组#####
VLgroup.vs.llv.vf <- TreatMore6ms %>% left_join(ids.select.group, by = "ID")
#####2.5计算入组时间#####
#第一次记录VL不是大于等于1000则删除，只有VF可以通过一次判断入组，其余都要观察两次，根据第二次值入组
VLgroup.vs.llv.vf.follow <- VLgroup.vs.llv.vf %>% group_by(ID) %>% arrange(ID, TestingDate) %>%
  mutate(row_number = row_number()) %>%
  filter(!(VLgroup == "VF" & row_number == 1 & VL < 1000)) %>%
  filter(!(VLgroup == "VS" & row_number == 1)) %>%
  filter(!(VLgroup == "LLV" & row_number == 1)) %>% 
  select(-row_number)%>%
  mutate(StartfollowDate = first(VLDate[!is.na(VLDate)]))%>% 
  mutate(FollowTime.m = round(interval(start=StartfollowDate, end=TestingDate) / months(1), digits = 2)) %>% 
  mutate(ART6months = ifelse(ART6months== -1, NA, ART6months)) %>% 
  filter(FollowTime.m >= 0)



####3.入组后开始观察####
#####3.1血糖#####
#去除血糖缺失值
Gly <- VLgroup.vs.llv.vf.follow %>% filter(!is.na(Gly))
#识别基线血糖异常的ID
abnormalGLY <- Gly %>% group_by(ID) %>% arrange(ID, TestingDate) %>%
  slice(1) %>% filter(Gly >= 7 | Gly <= 2.8) %>% pull(ID)
#剔除基线血糖异常的ID,对随访的血糖异常值进行剔除
Gly <- Gly %>% filter(!ID %in% abnormalGLY) %>% filter(Gly>2.8&Gly<12) %>% filter(n()>2)
write.xlsx(Gly, "Gly.xlsx")
# hist(data.3.3.3$Gly, breaks = 120)
# boxplot(data.3.3.3$Gly)
# summary(data.3.3.3$Gly)
#IQR <- Q3 - Q1
#upper.IQR <- Q3 + 1.5 * IQR
#hist(data.hlme$Gly, breaks = 30)

#####3.2检测时间间隔#####
#去除检测时间间隔小于等于30天的数据，剔除检测时间间隔大于180天往后的数据
Gly.TestInterval.30.180 <- Gly %>% group_by(ID) %>% arrange(ID, TestingDate) %>%
  mutate(Testintervalday = c(0, as.numeric(diff(TestingDate)))) %>% 
  filter(row_number() == 1 & Testintervalday == 0 | Testintervalday > 30) %>% filter(n()>2) %>% 
  mutate(Testintervalday = c(0, as.numeric(diff(TestingDate)))) %>% 
  filter(row_number() == 1 & Testintervalday == 0 | Testintervalday > 30) %>% filter(n()>2) %>% 
  mutate(Testintervalday = c(0, as.numeric(diff(TestingDate)))) %>% 
  mutate(Testintervalday = c(0, as.numeric(diff(TestingDate)))) %>% 
  mutate(FollowStatus = ifelse(Testintervalday > 180, "N", "Y")) %>% 
  mutate(cumsum_status = cumsum(FollowStatus == "N")) %>%
  filter(cumsum_status == 0) %>%
  select(-cumsum_status) %>% 
  filter(n()>2)

####4.观察至第一次发生血糖异常截至####
Gly.Observe.to.abnormal <- Gly.TestInterval.30.180 %>% group_by(ID) %>% arrange(ID, TestingDate) %>%
  filter((cumsum(Gly >= 7) == 0 | (Gly >= 7 & cumsum(Gly >= 7) == 1))) %>% 
  filter(FollowTime.m <= 84) %>% filter(n()>2)

To.abnormal.glygroup <- Gly.Observe.to.abnormal %>% group_by(ID)%>%mutate(
  Glygroup = case_when(any(Gly >= 7) ~ 1, TRUE ~ 0))
write.xlsx(To.abnormal.glygroup, "To.abnormal.glygroup.xlsx")

#####4.1制作VLgroup与Glygroup的统计交叉表
#简化数据框
vl.gly.group <- To.abnormal.hlme %>% ungroup(ID) %>% select(1,3,5) %>% distinct(ID, .keep_all = TRUE)
str(vl.gly.group)
vl.gly.group$Glygroup <- as.factor(vl.gly.group$Glygroup)
#制交叉表
table.vl.gly <- table(vl.gly.group$VLgroup, vl.gly.group$Glygroup)
percent <- prop.table(table.vl.gly, margin = 1) * 100
#将values转化为data
data.vl.gly <- as.data.frame(table.vl.gly)
data.vl.gly$Var1 <- as.character(data.vl.gly$Var1)
#循环统计频数及其百分比表格
Freq.percent <- table.vl.gly
for (i in 1:nrow(table.vl.gly)) {
  for (j in 1:ncol(table.vl.gly)) {
    Freq.percent[i, j] <- paste0(table.vl.gly[i, j], " (", round(percent[i, j], 2), "%)")
  }
}
#将values转化为data
Freq.percent <- as.data.frame.matrix(Freq.percent)
#添加Total
rowtotals <- rowSums(table.vl.gly)
coltotals <- colSums(table.vl.gly)
Freq.percent$Total <- rowtotals
Freq.percent <- rbind(Freq.percent, Total = c(coltotals, sum(coltotals)))
Freq.percent <- Freq.percent %>% select(`1`, `0`,`Total`)  #重新排列顺序
print(Freq.percent)

#总体卡方检验
chisq.result <- chisq.test(table.vl.gly)
print(chisq.result)

#计算相关系数
library(vcd)
assocstats(table.vl.gly)
#Contingency coefficient (列联系数)
#Cramer’s V (Cramer V系数)



library(jmv)
#两两比较：P值和关联强度
#P值
data.llv.vf <- data.vl.gly  %>% filter(Var1 !="VS")
data.llv.vs <- data.vl.gly  %>% filter(Var1 !="VF")
data.vf.vs <- data.vl.gly  %>% filter(Var1 !="LLV")
p.llv.vf <- contTables(formula = Freq ~ Var1:Var2, data = data.llv.vf, contCoef = TRUE, phiCra = TRUE)
p.llv.vs <- contTables(formula = Freq ~ Var1:Var2, data = data.llv.vs, contCoef = TRUE, phiCra = TRUE)
p.vf.vs <- contTables(formula = Freq ~ Var1:Var2, data = data.vf.vs, contCoef = TRUE, phiCra = TRUE)
print(p.llv.vf)
print(p.llv.vs)
print(p.vf.vs)

#OR值
library(epiDisplay)
tab.llv.vf <- xtabs(Freq~Var1+Var2, data = data.llv.vf)
tab.llv.vf <- tab.llv.vf[, c("1", "0")]#重新更改列的位置，把1放在前面，因为OR值计算是（a/b)/(c/d)=ad/bc
tab.llv.vs <- xtabs(Freq~Var1+Var2, data = data.llv.vs)
tab.llv.vs <- tab.llv.vs[, c("1", "0")]
tab.vf.vs <- xtabs(Freq~Var1+Var2, data = data.vf.vs)
tab.vf.vs <- tab.vf.vs[, c("1", "0")]

cci(cctable = tab.llv.vf)
cci(cctable = tab.llv.vs)
cci(cctable = tab.vf.vs)




####5.生存分析####
library(survival)
library(survminer)
#生成SurvivalTime
Glygroup.surv <- To.abnormal.glygroup %>% group_by(ID) %>% 
  mutate(Survivaltime = max(FollowTime.m))%>% 
  select(1, 6, 11, 16,17) %>% 
  distinct(ID, .keep_all = TRUE)
write.xlsx(Glygroup.surv, "Glygroup.surv.xlsx")

#拟合生存函数
fit <- survfit(Surv(Survivaltime,Glygroup) ~ VLgroup,
               data = Glygroup.surv)
fit

#绘制风险函数图
ggsurvplot(
  fit,
  data = Glygroup.surv,
  fun = "cumhaz",#将生存曲线转化为风险曲线
  linetype = 1, # 根据分层更改线型c(0,1) or  c("solid", "dashed") or "strata"
  #surv.median.line = "hv", # 同时显示垂直和水平参考线 即增加中位生存时间 可选 "none"、"hv"、"h"、"v"
  palette = "lancet" ,#定义颜色 可选调色板有 "hue" "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty".
  xlab = "Time(months)",
  ylab = "Cumulative Hazard of Hyperglycemia",
  title = "",
  legend = "right", # 指定图例位置 "top"(默认),"left","right","none","bottom"
  legend.title = "",
  legend.labs = c("VS", "LLV", "VF") ,
  xlim = c(0, 90),
  ylim = c(0, 1),
  break.x.by = 12,
  break.y.by = .25,
  axes.offset = F, # 逻辑词，默认为TRUE。FALSE则生存曲线图的坐标轴从原点开始
  #conf.int = TRUE,#增加置信区间
  #conf.int.alpha = .3,# 数值，指定置信区间填充颜色的透明度； # 数值在0-1之间，0为完全透明，1为不透明
  pval = TRUE, #log 秩检验
  pval.size = 5,# 指定p值文本大小的数字，默认为 5
  pval.coord = c(1,0.24),# 长度为2的数字向量，指定p值位置x、y，如pval.coord=c(x,y)
  censor = T, # 逻辑词，默认为TRUE，在图上绘制删失点。
  censor.shape = 3, # 数值或字符，用于指定删失点的形状；默认为"+"(3), 可选"|"(124)
  censor.size = 1.5,# 指定删失点形状的大小，默认为4.5
  # risk.table = "absolute", #"absolute"、"percentage"、"abs_pct"", #绝对人数、百分比和危险之中
  # #risk.table.pos =in，#分线表放在图内
  # risk.table.col = "strata", #按组更改风险表颜色
  # risk.table.y.text.col = TRUE, #颜色风险表文本注释（按层）
  # risk.table.y.text = FALSE, #在风险表图例中的文本注释中显示条形而不是名称
  # risk.table.height = 0.2,  
  # risk.table.title="Number at the time",
  # fontsize=4,#风险表字体
  #ggtheme = theme_bw(), #使用主题自定义情节和风险表。
  ggtheme = theme_survminer()+
    theme(
      panel.grid.major = element_line(color = "gray", size = 0.5),  # 设置主要网格线
      panel.grid.minor = element_line(color = "gray", linetype = "dotted", size = 0.25)) # 设置次要网格线
)

#观察结果统计
To.abnormal.glygroup.uniqueID <- To.abnormal.glygroup %>% distinct(ID, .keep_all = TRUE)
table(To.abnormal.glygroup.uniqueID$Glygroup)
table(To.abnormal.glygroup.uniqueID$VLgroup)




####6.lcmm####
library(lcmm)
To.abnormal.hlme <- To.abnormal.glygroup %>% select( 1, 6, 11, 13, 16) %>% group_by(ID) %>% 
  arrange(ID, FollowTime.m)%>% 
  mutate(Subject = cur_group_id())

model.1 <- hlme(fixed = Gly ~ 1 + FollowTime.m + I(FollowTime.m^2),
                random = ~ 1 + FollowTime.m,
                ng = 1, 
                data = To.abnormal.hlme, 
                subject = "Subject")
summary(model.1)
model.2 <- hlme(fixed = Gly ~ 1 + FollowTime.m + I(FollowTime.m^2),
                mixture = ~ 1 + FollowTime.m + I(FollowTime.m^2),
                random = ~ 1 + FollowTime.m,
                ng = 2,
                nwg = TRUE, 
                B = model.1,
                idiag = FALSE, 
                maxiter = 50,
                data = To.abnormal.hlme, 
                subject = "Subject")
summary(model.2)
postprob(model.2)

windows(width=10, height=8)
datnew  <- data.frame(FollowTime.m = seq(0,84, length = 1000))
plotpred <- predictY(model.3, datnew, var.time ="FollowTime.m")
plot(plotpred, lty = 1, lwd = 5,marg=FALSE, shades=T,
     xlab="FollowTime(Months)", ylab="GLY", 
     legend.loc = "topleft", cex=0.75)



