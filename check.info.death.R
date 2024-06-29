library(tidyverse)
library(openxlsx)
library(readxl)
library(dplyr)
library(lubridate)

####1.check,info.ARTInitialDate####
fcg.check <- read_excel("FCG.xlsx", sheet = "check")
str(fcg.check)
fcg.check$VL <- as.numeric(fcg.check$VL)
fcg.check$ID <- gsub("'",'',fcg.check$ID)
fcg.check$Key <- gsub("'",'',fcg.check$Key)
summary(fcg.check$VL)
fcg.info <- read_excel("FCG.xlsx", sheet = "info")
str(fcg.info)
fcg.info$ARTinitialDate <-  as.Date(fcg.info$ARTinitialDate)
fcg.info <- fcg.info %>% distinct(ID, .keep_all = TRUE)

fcg <- fcg.check %>% left_join(fcg.info, by = "ID")
str(fcg)
summary(fcg$VL)


qz.check <- read_excel("QZ.xlsx", sheet = "check")
qz.check$VL <- as.numeric(qz.check$VL)
qz.check$ID <- gsub("'",'',qz.check$ID)
qz.check$Key <- gsub("'",'',qz.check$Key)

qz.info <- read_excel("QZ.xlsx", sheet = "info")
str(qz.info)
qz.info$ARTinitialDate <- as.Date(qz.info$ARTinitialDate)
qz.info <- qz.info %>% distinct(ID, .keep_all = TRUE)
qz.info$ID <- gsub("'",'',qz.info$ID)
qz.info$Key <- gsub("'",'',qz.info$Key)
qz <- qz.check %>% left_join(qz.info, by = "ID")
str(qz)
summary(qz$VL)


cz.check <- read_excel("CZ.xlsx", sheet = "check")
cz.check$VL <- as.numeric(cz.check$VL)
cz.check$ID <- gsub("'",'',cz.check$ID)
cz.check$Key <- gsub("'",'',cz.check$Key)
cz.info <- read_excel("CZ.xlsx", sheet = "info")
str(cz.info)
cz.info$ARTinitialDate <- as.Date(cz.info$ARTinitialDate)
cz.info <- cz.info %>% distinct(ID, .keep_all = TRUE)

cz <- cz.check %>% left_join(cz.info, by = "ID")
str(cz)
cz$TestingDate <- as.Date(cz$TestingDate)
cz$VLDate <- as.Date(cz$VLDate)
cz$Gly <- as.numeric(cz$Gly)
summary(cz$VL)


nn.check <- read_excel("NN.xlsx", sheet = "check")
nn.check$VL <- as.numeric(nn.check$VL)
nn.check$Gly <- as.numeric(nn.check$Gly)
nn.check$TestingDate <- as.Date(nn.check$TestingDate)
nn.check$VLDate <- as.Date(nn.check$VLDate)
str(nn.check)
nn.info <- read_excel("NN.xlsx", sheet = "info")
str(nn.info)
nn.info$ARTinitialDate <- as.Date(nn.info$ARTinitialDate)
nn.info <- nn.info %>% distinct(ID, .keep_all = TRUE)

nn <- nn.check %>% left_join(nn.info, by = "ID")
str(nn)
nn$VLDate <- as.character(nn$VLDate)
summary(nn$VL)


write.xlsx(fcg, "fcg.R.xlsx")
write.xlsx(qz, "qz.R.xlsx")
write.xlsx(cz, "cz.R.xlsx")
write.xlsx(nn, "nn.R.xlsx")
str(fcg)
str(qz)
str(cz)
str(nn)

all <- bind_rows(fcg, qz, cz, nn)
str(all)

write.xlsx(all, "all.xlsx")

fcg <- read_excel("fcg.R.xlsx")
qz <- read_excel("qz.R.xlsx")
cz <- read_excel("cz.R.xlsx")
nn <- read_excel("nn.R.xlsx")


####2.death####
fcg.death <-read_excel("FCG.xlsx", sheet = "death") 
fcg.death$DiedDate <- as.Date(fcg.death$DiedDate)
str(fcg.death)

cz.death <- read_excel("CZ.xlsx", sheet = "death")
cz.death$DiedDate <- as.Date(cz.death$DiedDate)
str(cz.death)

qz.death <- read_excel("QZ.xlsx", sheet = "death")
qz.death$DiedDate <- as.Date(qz.death$DiedDate)
str(qz.death)

nn.death <- read_excel("NN.xlsx", sheet = "death")
nn.death$DiedDate <- as.Date(nn.death$DiedDate)
str(nn.death)

death <- bind_rows(fcg.death, cz.death, qz.death, nn.death)
death <- death %>% distinct(ID, .keep_all = TRUE)
write.xlsx(death, "death.xlsx")

####3.info.Regimen####
fcg.info <- read_excel("FCG.xlsx", sheet = "info.1")
nn.info <- read_excel("NN.xlsx", sheet = "info.1")
cz.info <- read_excel("CZ.xlsx", sheet = "info.1")
qz.info <- read_excel("QZ.xlsx", sheet = "info.1", 
                      col_types = c("text", "text", "text", "date", "text", "text", "text", "text", "text", "text"))
qz.info$BirthDate <- as.character(qz.info$BirthDate)
str(qz.info)
write.xlsx(fcg.info, "fcg.info.xlsx")
write.xlsx(nn.info, "nn.info.xlsx")
write.xlsx(cz.info, "cz.info.xlsx")
write.xlsx(qz.info, "qz.info.xlsx")

info <- bind_rows(fcg.info, nn.info, qz.info, cz.info)
info$ID <- gsub("'",'',info$ID)
info <- info %>% distinct(ID, .keep_all = TRUE)
write.xlsx(info, "info.xlsx")