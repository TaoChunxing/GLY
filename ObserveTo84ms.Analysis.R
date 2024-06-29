library(tidyverse)
library(openxlsx)
library(readxl)
library(dplyr)
library(lubridate)

setwd("E:/Molecular Epidemiology/GLYCEMIA/GLY")
all <- read_excel("all.xlsx")
str(all)
model.3 <- readRDS("model.3.rds")
summary(model.3)
postprob(model.3)
####1.定位到治疗半年以上的数据####
#####1.1找到治疗半年以上的病载记录，去除半年以前的记录#
TreatMore6ms <- all %>% group_by(ID) %>% arrange(ID, TestingDate) %>% 
  mutate(TimefromART.m = round(interval(start=ARTinitialDate, end=VLDate) / months(1), digits = 2)) %>% 
  mutate(ART6months = ifelse(!is.na(VL) & TimefromART.m > 6, cumsum(!is.na(VL) & TimefromART.m > 6),NA)) %>% 
  mutate(first_occurrence = if_else(any(ART6months == 1), which(ART6months == 1)[1], NA_integer_)) %>%
  filter(is.na(first_occurrence) | row_number() >= first_occurrence) %>% 
  select(-first_occurrence) %>% 
  filter(!all(is.na(ART6months))) %>% 
  mutate(ART6months = ifelse(is.na(ART6months), -1, ART6months))
#对治疗半年以上只有一次记录的ID，若不是VF则去除,因为两次记录才能判定LLV和VS
ids.1record <- TreatMore6ms %>% group_by(ID) %>% filter(!any(ART6months == 2))
ids.1record.removed <- ids.1record %>% group_by(ID) %>% filter(any(ART6months == 1 & VL< 1000))
TreatMore6ms <- anti_join(TreatMore6ms, ids.1record.removed, by = "ID")


####2.VLgroup,分VS,LLV,VF组####
#####2.1提取出只有一条记录的ID，分组为VF#####
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

ids.2record.select <- ids.2record %>% select(1, 11) %>% distinct(ID, .keep_all = TRUE)
ids.1record.keep.select <- ids.1record.keep %>% select(1, 11) %>% distinct(ID, .keep_all = TRUE)
ids.select.group <- bind_rows(ids.2record.select, ids.1record.keep.select)

VLgroup.vs.llv.vf <- TreatMore6ms %>% left_join(ids.select.group, by = "ID")

#####2.3计算入组时间#####
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
write.xlsx(VLgroup.vs.llv.vf.follow, "VLgroup.vs.llv.vf.follow.xlsx")

#####2.4去除入组为VF的数据#####
VLgroup.vs.llv.follow  <-  VLgroup.vs.llv.vf.follow %>% filter(VLgroup!="VF")
write.xlsx(VLgroup.vs.llv.follow, "VLgroup.vs.llv.follow.xlsx")


####3.入组后开始观察####
#####3.1血糖#####
#去除血糖缺失值
Gly <- VLgroup.vs.llv.follow %>% filter(!is.na(Gly))
#识别基线血糖异常的ID
abnormalGLY <- Gly %>% group_by(ID) %>% arrange(ID, TestingDate) %>%
  slice(1) %>% filter(Gly >= 7 | Gly <= 2.8) %>% pull(ID)
#剔除基线血糖异常的ID,对随访的血糖异常值进行剔除
Gly <- Gly %>% filter(!ID %in% abnormalGLY) %>% filter(Gly>1&Gly<33) %>% filter(n()>2)
write.xlsx(Gly, "Gly.xlsx")
# hist(data.3.3.3$Gly, breaks = 120)
# boxplot(data.3.3.3$Gly)
# summary(data.3.3.3$Gly)
#IQR <- Q3 - Q1
#upper.IQR <- Q3 + 1.5 * IQR
#hist(data.hlme$Gly, breaks = 30)

#####3.2检测时间间隔####
#去除检测时间间隔小于等于30天的数据,去除检测时间间隔小于等于180天往后的数据
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
write.xlsx(Gly.TestInterval.30.180, "Gly.TestInterval.30.180.xlsx")

#####3.3观察终点#####
#统一观察7年截至，不管有没有发生血糖异常
Gly.observe.to.84ms <- Gly.TestInterval.30.180 %>% 
  group_by(ID) %>% arrange(ID, TestingDate) %>% 
  filter(FollowTime.m <= 84) %>% filter(n()>2)
write.xlsx(Gly.observe.to.84ms, "Gly.observe.to.84ms.xlsx")

#观察7年后，连续出现2次血糖异常为1，未出现为0
To.84ms.glygroup <- Gly.observe.to.84ms %>% group_by(ID) %>% mutate(
  Glygroup = ifelse(any(Gly >= 7 & lead(Gly, 1) >= 7, na.rm = TRUE), 1, 0))
To.84ms.glygroup.unique <- To.84ms.glygroup %>% distinct(ID, .keep_all = TRUE)
table(To.84ms.glygroup.unique$VLgroup)
table(To.84ms.glygroup.unique$Glygroup)


####4.轨迹组成员生存分析前数据准备####
#获取轨迹组内的ID的原本完整的TestingDate,因为筛选后的TestingDate不完整
ids.to84ms <- To.84ms.glygroup.unique %>% pull(ID)
To.84ms.survival <- all %>% filter(ID %in% ids.to84ms)
info <- To.84ms.glygroup.unique %>% select(1, 11, 12, 13, 16)
#合并死亡日期到轨迹组成员内
To.84ms.died <- left_join(To.84ms.survival, death, by = "ID")
#获取轨迹组内的ID的StartfollowDate等信息
To.84ms.died.survtime <- To.84ms.died %>% left_join(info, by= "ID")%>% 
  group_by(ID) %>% arrange(ID, TestingDate) %>% 
  mutate(SurvTime.m = ifelse(
    is.na(DiedDate),
    round(interval(start=StartfollowDate, end=max(TestingDate)) / months(1), digits = 2),
    round(interval(start=StartfollowDate, end=DiedDate) / months(1), digits = 2)))

To.84ms.died.survtime.Status <- To.84ms.died.survtime %>% 
  mutate(SurvivalStatus=ifelse(is.na(DiedDate), 0, 1)) %>% 
  mutate(SurvivalStatus.a=ifelse(SurvivalStatus==1&SurvTime.m>=84, 0, SurvivalStatus))%>% 
  mutate(SurvTime.m.a = ifelse(SurvTime.m>=84,84,SurvTime.m))
#保留一条ID记录并简化数据框
To.84ms.surv.unique <- To.84ms.died.survtime.Status %>% group_by(ID) %>%
  distinct(ID, .keep_all = TRUE) %>% 
  select(1, 13, 16, 17, 19,20)

#剔除记录错误的id(检测时间晚于死亡时间的ID)
ids.testlaterdeath <- To.84ms.surv.unique %>% filter(SurvTime.m.a<=0) %>% pull(ID)
To.84ms.surv.unique <- To.84ms.surv.unique %>% filter(!ID %in% ids.testlaterdeath)
summary(To.84ms.surv.unique$SurvTime.m.a)
table(To.84ms.surv.unique$SurvivalStatus.a)

To.84ms.glygroup <- To.84ms.glygroup %>% filter(!ID %in% ids.testlaterdeath)
To.84ms.glygroup.unique <- To.84ms.glygroup.unique %>% filter(!ID %in% ids.testlaterdeath)
write.xlsx(To.84ms.glygroup, "To.84ms.glygroup.xlsx")
write.xlsx(To.84ms.glygroup.unique, "To.84ms.glygroup.unique.xlsx")
####5.lcmm识别不同轨迹组####
library(lcmm)
#####5.1简化数据框，生成Subject#####
To.84ms.hlme <- To.84ms.glygroup %>% select( 1, 6, 11, 13, 16) %>% 
  group_by(ID) %>% arrange(ID, FollowTime.m)%>% 
  mutate(Subject = cur_group_id())
write.xlsx(To.84ms.hlme, "To.84ms.hlme.xlsx")
#####5.2计算模型#####
model.1 <- hlme(fixed = Gly ~ 1 + FollowTime.m + I(FollowTime.m^2),
                random = ~ 1 + FollowTime.m,
                ng = 1, 
                data = To.84ms.hlme, 
                subject = "Subject")
summary(model.1)
model.4 <- hlme(fixed = Gly ~ 1 + FollowTime.m + I(FollowTime.m^2),
                mixture = ~ 1 + FollowTime.m + I(FollowTime.m^2),
                random = ~ 1 + FollowTime.m,
                ng = 4,
                nwg = TRUE, 
                B = model.1,
                idiag = FALSE, 
                maxiter = 500,
                data = To.84ms.hlme, 
                subject = "Subject")
summary(model.4)
postprob(model.4)
#####5.3绘图#####
windows(width=10, height=8)
datnew  <- data.frame(FollowTime.m = seq(0,84, length = 1000))
plotpred <- predictY(model.2, datnew, var.time ="FollowTime.m")
plot(plotpred, lty = 1, lwd = 5,marg=FALSE, shades=T,
     xlab="FollowTime(Months)", ylab="GLY", 
     legend.loc = "topleft", cex=0.75)

#####5.4提取不同轨迹组的对象#####
class <- model.3$pprob[,1:2]
#####5.5合并hlme与class#####
To.84ms.hlme.class <- To.84ms.hlme %>% left_join(class, by = "Subject")
To.84ms.hlme.class.unique <- To.84ms.hlme.class %>% distinct(ID, .keep_all = TRUE)

#####5.6制作VLgroup与class的交叉表#####
table <- table(To.84ms.hlme.class.unique$VLgroup, To.84ms.hlme.class.unique$class)
percent <- prop.table(table, margin = 1) * 100
count.percent <- table
for (i in 1:nrow(table)) {
  for (j in 1:ncol(table)) {
    count.percent[i, j] <- paste0(table[i, j], " (", round(percent[i, j], 2), "%)")
  }
}
count.percent <- as.data.frame.matrix(count.percent)
rowtotals <- rowSums(table)
coltotals <- colSums(table)
count.percent$Total <- rowtotals
count.percent <- rbind(count.percent, Total = c(coltotals, sum(coltotals)))
#count.percent <- count.percent %>% select(`1`, `0`,`Total`)
count.percent



####6.不同轨迹组内成员的死亡生存分析####
library(survival)
library(survminer)

#####6.1合并生存分析准备数据框与lcmm模型生成的class#####
To.84ms.surv.unique.class <- To.84ms.surv.unique %>% 
  left_join(To.84ms.hlme.class.unique.select, by = "ID")

#####6.2拟合生存函数#####
fit <- survfit(Surv(SurvTime.m.a,SurvivalStatus.a) ~ class,
               data = To.84ms.surv.unique.class)
fit
saveRDS(fit, "survfit.rds")

#####6.3绘制不同class内成员的死亡生存曲线#####
ggsurvplot(
  fit,
  data = To.84ms.surv.unique.class,
  #fun = "cumhaz",#将生存曲线转化为风险曲线
  linetype = 1, # 根据分层更改线型c(0,1) or  c("solid", "dashed") or "strata"
  #surv.median.line = "hv", # 同时显示垂直和水平参考线 即增加中位生存时间 可选 "none"、"hv"、"h"、"v"
  palette = "lancet" ,#定义颜色 可选调色板有 "hue" "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty".
  xlab = "Time(months)",
  ylab = "Survival Probability",
  title = "",
  legend = "right", # 指定图例位置 "top"(默认),"left","right","none","bottom"
  legend.title = "",
  legend.labs = c("class1", "class2"),
  xlim = c(0,85),
  ylim = c(0.75,1),
  break.x.by = 12,
  break.y.by = .05,
  axes.offset = F, # 逻辑词，默认为TRUE。FALSE则生存曲线图的坐标轴从原点开始
  #conf.int = TRUE,#增加置信区间
  #conf.int.alpha = .3,# 数值，指定置信区间填充颜色的透明度； # 数值在0-1之间，0为完全透明，1为不透明
  pval = TRUE, #log 秩检验
  pval.size = 5,# 指定p值文本大小的数字，默认为 5
  pval.coord = c(6,0.8),# 长度为2的数字向量，指定p值位置x、y，如pval.coord=c(x,y)
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
  # ggtheme = theme_bw(), #使用主题自定义情节和风险表。
   ggtheme = theme_survminer()+
     theme(
       panel.grid.major = element_line(color = "gray", size = 0.5),  # 设置主要网格线
       panel.grid.minor = element_line(color = "gray", linetype = "dotted", size = 0.25)) # 设置次要网格线
)
 