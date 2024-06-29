library(tidyverse)
library(openxlsx)
library(readxl)
library(dplyr)
library(lubridate)

info <- read_excel("info.xlsx")
info <- info %>% distinct(ID, .keep_all = TRUE)
info <- subset(info, !is.na(InitialRegimen))
info$InitialRegimen <- gsub("双汰芝","AZT+3TC", info$InitialRegimen)
info$InitialRegimen <- gsub("克力芝","LPV/r", info$InitialRegimen)
info$InitialRegimen <- gsub("/", "+", info$InitialRegimen)

NRTIs <- c("3TC","D4T", "TDF", "TAF", "FTC", "AZT", "DDI")
NNRTIs <- c("EFV", "RPV", "DOR","NVP")
PIs <- c("LPV", "r", "DRV", "c", "NFV", "ANV")
INSTIs <- c("DTG", "RAL", "BIC", "EVG")

f.Regimenbased <- function(regimen) {
  drugs <- unlist(str_split(regimen, "\\+"))
  if (any(drugs %in% NNRTIs)) {
    return("NNRTIs-based")
  } else if (any(drugs %in% PIs)) {
    return("PIs-based")
  } else if (any(drugs %in% INSTIs)) {
    return("INSTIs-based")
  } else {
    return("Other/combinations")
  }
}

info.Regimenbased <- info %>%
  mutate(Regimenbased = sapply(InitialRegimen, f.Regimenbased))

write.xlsx(info.Regimenbased, "info.Regimenbased.xlsx")

To.84ms.glygroup.unique <- read_excel("To.84ms.glygroup.unique.xlsx")
glygroup.Regimenbased <- To.84ms.hlme.unique %>% left_join(info.Regimenbased, by="ID")

glygroup.Regimenbased <- glygroup.Regimenbased %>%
  mutate(Regimenbased.a = case_when(
    Regimenbased == "NNRTIs-based" ~ 1,
    Regimenbased == "PIs-based" ~ 2,
    Regimenbased == "INSTIs-based" ~ 3,
    Regimenbased == "Other/combinations" ~ 4,
    TRUE ~ NA_real_  ))
glygroup.Regimenbased <- subset(glygroup.Regimenbased, !is.na(Regimenbased.a))
table(glygroup.Regimenbased$Regimenbased.a)
glygroup.Regimenbased <- glygroup.Regimenbased %>%
  mutate(VLgroup.a = case_when(
    VLgroup == "LLV" ~ 1,
    VLgroup == "VS" ~ 0))
####4.PSM匹配####
library(MatchIt)

#####4.1Nearest Neighbor Matching#####
#HIV
matchlist <- matchit(VLgroup.a ~ Regimenbased.a,
                     data = glygroup.Regimenbased,
                     method = "nearest",
                     distance = "glm",
                     caliper = 0.01,
                     ratio = 4,
                     replace = F)
summary(matchlist)
#####4.2提取匹配后的数据#####
matchdata <- match.data(matchlist,
                        group = "all",
                        distance = "distance",
                        weights = "weights",
                        subclass = "subclass",
                        data = NULL,
                        include.s.weights = TRUE,
                        drop.unmatched = TRUE)
#检验VLgroup与Regimen之间是否还有统计学差异（无）
table <- table(matchdata$VLgroup.a, matchdata$Regimenbased.a)
table
chi.result <- chisq.test(table)
chi.result
#Warning message:
#In chisq.test(table) : Chi-squared近似算法有可能不准,改用Fisher 精确检验
fisher.result <- fisher.test(table)
fisher.result
#检验VLgroup与Glygroup之间是否有统计学差异（有）
table <- table(matchdata$VLgroup.a, matchdata$Glygroup)
table
chi.result <- chisq.test(table)
chi.result
write.xlsx(matchdata,"matchdata.xlsx")

####lcmm####
matchID <- matchdata %>% pull(ID)
match.hlme <- To.84ms.hlme %>% filter(ID %in% matchID)

library(lcmm)
model.1 <- hlme(fixed = Gly ~ 1 + FollowTime.m + I(FollowTime.m^2),
                random = ~ 1 + FollowTime.m,
                ng = 1, 
                data = match.hlme, 
                subject = "Subject")
summary(model.1)
model.2 <- hlme(fixed = Gly ~ 1 + FollowTime.m + I(FollowTime.m^2),
                mixture = ~ 1 + FollowTime.m + I(FollowTime.m^2),
                random = ~ 1 + FollowTime.m,
                ng = 3,
                nwg = TRUE, 
                B = model.1,
                idiag = FALSE, 
                maxiter = 500,
                data = match.hlme, 
                subject = "Subject")
summary(model.2)
postprob(model.2)

#####5.3绘图#####
windows(width=10, height=8)
datnew  <- data.frame(FollowTime.m = seq(0,84, length = 1000))
plotpred <- predictY(model.3, datnew, var.time ="FollowTime.m")
plot(plotpred, lty = 1, lwd = 5,marg=FALSE, shades=T,
     xlab="FollowTime(Months)", ylab="GLY", 
     legend.loc = "topleft", cex=0.75)

#####5.4提取不同轨迹组的对象#####
class <- model.3$pprob[,1:2]
#####5.5合并hlme与class#####
To.84ms.match.hlme.class <- To.84ms.match.hlme %>% left_join(class, by = "Subject")
To.84ms.match.hlme.class.unique <- To.84ms.match.hlme.class %>% distinct(ID, .keep_all = TRUE)

#####5.6制作VLgroup与class的交叉表#####
table <- table(To.84ms.match.hlme.class.unique$VLgroup, To.84ms.match.hlme.class.unique$class)
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


To.84ms.glygroup.surv.last <- read.xlsx("To.84ms.glygroup.surv.last.xlsx")
match.surv <- To.84ms.glygroup.surv.last %>% filter(ID %in% matchID)
match.surv$VLgroup <- as.factor(match.surv$VLgroup)
match.surv$VLgroup <- relevel(match.surv$VLgroup, ref = "VS")
#单因素cox回归
library(survival)
library(survminer)
f <- coxph(Surv(FollowTime.m, Glygroup) ~ VLgroup, data = match.surv)
write_rds(f,"match.coxph.rds")
#计算HR
summary.f <- summary(f)
HR <- summary.f$coef[1, "exp(coef)"]
HR.lower <- summary.f$conf.int[1, "lower .95"]
HR.upper <- summary.f$conf.int[1, "upper .95"]
HR.CI <- paste0("HR = ", round(HR, 2), " (95% CI: ", round(HR.lower, 2), "-", round(HR.upper, 2), ")")

#拟合血糖风险函数
fit <- survfit(Surv(FollowTime.m,Glygroup) ~ VLgroup,
               data = match.surv)
fit
write_rds(fit,"match.survfit.rds")
#画血糖风险函数图
ggsurv <- ggsurvplot(
  fit,
  data = match.surv,
  fun = "cumhaz",#将生存曲线转化为风险曲线
  linetype = 1, # 根据分层更改线型c(0,1) or  c("solid", "dashed") or "strata"
  #surv.median.line = "hv", # 同时显示垂直和水平参考线 即增加中位生存时间 可选 "none"、"hv"、"h"、"v"
  palette = "lancet" ,#定义颜色 可选调色板有 "hue" "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty".
  xlab = "FollowTime(Months)",
  ylab = "Cumulative Hazard Rate of Hyperglycemia",
  title = "",
  legend = "right", # 指定图例位置 "top"(默认),"left","right","none","bottom"
  legend.title = "",
  legend.labs = c("LLV", "VS"),
  xlim = c(0,85),
  ylim = c(0,0.5),
  break.x.by = 12,
  break.y.by = .05,
  axes.offset = F, # 逻辑词，默认为TRUE。FALSE则生存曲线图的坐标轴从原点开始
  conf.int = TRUE,#增加置信区间
  conf.int.alpha = .3,# 数值，指定置信区间填充颜色的透明度； # 数值在0-1之间，0为完全透明，1为不透明
  pval = TRUE, #log 秩检验
  pval.size = 5,# 指定p值文本大小的数字，默认为 5
  pval.coord = c(36,0.05),# 长度为2的数字向量，指定p值位置x、y，如pval.coord=c(x,y)
  censor = T, # 逻辑词，默认为TRUE，在图上绘制删失点。
  censor.shape = 3, # 数值或字符，用于指定删失点的形状；默认为"+"(3), 可选"|"(124)
  censor.size = 1.5,# 指定删失点形状的大小，默认为4.5
  risk.table = "absolute", #"absolute"、"percentage"、"abs_pct"", #绝对人数、百分比和危险之中
  risk.table.col = "strata", #按组更改风险表颜色
  risk.table.y.text.col = TRUE, #颜色风险表文本注释（按层）
  risk.table.y.text = FALSE, #在风险表图例中的文本注释中显示条形而不是名称
  risk.table.height = 0.2,  
  risk.table.title = "Number at the time",
  fontsize = 4,#风险表字体
  ggtheme = theme_bw()) #使用主题自定义plot和风险表
# ggtheme = theme_survminer())

ggsurv$plot <- ggsurv$plot +
annotate("text", x = 36, y = 0.1, label = HR.CI, size = 5, color = "black", hjust = 0)#hjust=0左对齐,0.5居中，1右对齐
ggsurv
