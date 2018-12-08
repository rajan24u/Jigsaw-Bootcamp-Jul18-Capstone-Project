

## Read Me-----------------------------------------------------------------------
# Script name: capstone_code_final_sumission
# Version: r1
# Author: Rajan Kumar
# Course: Jigsaw Bootcamp July 2018
# Model dataset (telecomfinal.csv) loacation: JLC Lab
# Period of development: 4th to 6th Oct 2018
# Note: This R script has been developed as a part Capstone Project submission for...
# ...Jigsaw Bootcamp July 2018 Certifiaction Course in Data Science and Machine Learning

## Load model input data set-----------------------------------------------------
getwd()
path = getwd()
list.files(path)
tel_data=read.csv("telecomfinal.csv") 
summary(tel_data)
str(tel_data)

## Load libraries----------------------------------------------------------------
library(dplyr)
library(lubridate)
library(dataQualityR)

## Create data quality reports---------------------------------------------------
checkDataQuality(tel_data, "sum_num.csv", "sum_cat.csv")
summ_num=read.csv("sum_num.csv") # continuous variable quality report/dataframe
head(summ_num)
View(summ_num)
summ_cat=read.csv("sum_cat.csv") # # categorical variable quality report/dataframe
head(summ_cat)
View(summ_cat)

## Quality Report Discussion-----------------------------------------------------
# The telecomfinal.csv has 66297 obs. of  81 variables...
# ...These 81 variables are split into 60 continuos and 21 categorical variables

# Remove variables having missing value greater than 25%-------------------------
num_miss_25 = filter(summ_num, missing.percent > 25)
num_miss_25$X # continuous variable names with missing value greater than 25%
nrow(num_miss_25) # 2 variables have missing value greatr than 25%
cat_miss_25 = filter(summ_cat, n.miss.percent > 25)
cat_miss_25$X # categorical variable names with missing value greater than 25%
nrow(cat_miss_25) # 11 variables have missing value greatr than 25%

tel_data_1 = select(tel_data,-c(dwlltype, dwllsize, mailordr, occu1, wrkwoman,
                                solflag, proptype, mailresp, cartype, 
                                children, div_type, numbcars))
# Variable "retdays" have only 3.25% i.e. 2154 obs having values. But this can be a significant...
#... variable. As adviced in data dictionary, the missing value can be assumed as customer...
#...who have not called customer support. Therefore we will impute value 0 to all missing values...
#...and update all non-na value to 1
summary(tel_data_1$retdays)
tel_data_1$retdays_up<-ifelse(is.na(tel_data_1$retdays), 0, 1)
class(tel_data_1$retdays_up)
#tel_data_1$retdays_up<-as.numeric(tel_data_1$retdays_up)
summary(tel_data_1$retdays_up) # no NA's
table(tel_data_1$retdays_up)
tel_data_1 = select(tel_data_1,-c(retdays)) # remove "retdays" and use "retdays_up"
ncol(tel_data_1) # updated dataset tel_data1 have 69 variables

## Missing value imputation------------------------------------------------------
# get continuous variable names having missing values
summ_num %>% filter(missing.percent <= 25 & missing.percent > 0) -> num_miss_0
num_miss_0$X # continuous variable names with missing value greater than 25%
nrow(num_miss_0) # 22 continuous variables need missing value imputation

# get categorical variable names having missing values
summ_cat %>% filter(n.miss.percent <= 25 & n.miss.percent > 0) -> cat_miss_0
cat_miss_0$X # categorical variable names with missing value greater than 25%
nrow(cat_miss_0) # 7 categorical variables need missing value imputation

# continuous variable missing value treatement: 22 variables + 2 variables with 1 NA 
# <1> mou_Mean
summary(tel_data_1$mou_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->mou_Mean_1
mou_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(mou_Mean,n=10))%>%
                        count(dec)%>%unname())[[2]]
mou_Mean_1$churn_perc<-mou_Mean_1$n/mou_Mean_1$N
mou_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(mou_Mean,n=10))%>%
                                  group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
mou_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(mou_Mean,n=10))%>%
                               group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
mou_Mean_1$varname<-rep("mou_Mean",nrow(mou_Mean_1))
mou_Mean_1 # NA churn_perc is not close to any decile
tel_data_1$mou_Mean[is.na(tel_data_1$mou_Mean)]<- 529.4 # imputing by mean
summary(tel_data_1$mou_Mean) # no NA's
num_miss_0$X

# <2> totmrc_Mean
summary(tel_data_1$totmrc_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->totmrc_Mean_1
totmrc_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%
                           count(dec)%>%unname())[[2]]
totmrc_Mean_1$churn_perc<-totmrc_Mean_1$n/totmrc_Mean_1$N
totmrc_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%
                                     group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
totmrc_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%
                                  group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
totmrc_Mean_1$varname<-rep("totmrc_Mean",nrow(totmrc_Mean_1))
totmrc_Mean_1 # NA churn_perc is not close to any decile
tel_data_1$totmrc_Mean[is.na(tel_data_1$totmrc_Mean)]<- 47.01 # imputing by mean
summary(tel_data_1$totmrc_Mean) # no NA's
num_miss_0$X

# <3> rev_Range
summary(tel_data_1$rev_Range) # 181 NA's
tel_data_1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->rev_Range_1
rev_Range_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(rev_Range,n=10))%>%
                           count(dec)%>%unname())[[2]]
rev_Range_1$churn_perc<-rev_Range_1$n/rev_Range_1$N
rev_Range_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(rev_Range,n=10))%>%
                                     group_by(dec)%>%summarise(min(rev_Range)))[[2]]
rev_Range_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(rev_Range,n=10))%>%
                                  group_by(dec)%>%summarise(max(rev_Range)))[[2]]
rev_Range_1$varname<-rep("rev_Range",nrow(rev_Range_1))
rev_Range_1 # NA churn_perc is not close to any decile
tel_data_1$rev_Range[is.na(tel_data_1$rev_Range)]<- 44.10 # imputing by mean
summary(tel_data_1$rev_Range) # no NA's
num_miss_0$X

# <4> mou_Range
summary(tel_data_1$mou_Range) # 181 NA's
tel_data_1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->mou_Range_1
mou_Range_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(mou_Range,n=10))%>%
                           count(dec)%>%unname())[[2]]
mou_Range_1$churn_perc<-mou_Range_1$n/mou_Range_1$N
mou_Range_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(mou_Range,n=10))%>%
                                     group_by(dec)%>%summarise(min(mou_Range)))[[2]]
mou_Range_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(mou_Range,n=10))%>%
                                  group_by(dec)%>%summarise(max(mou_Range)))[[2]]
mou_Range_1$varname<-rep("mou_Range",nrow(mou_Range_1))
mou_Range_1 # NA churn_perc is not close to any decile
tel_data_1$mou_Range[is.na(tel_data_1$mou_Range)]<- 376.5 # imputing by mean
summary(tel_data_1$mou_Range) # no NA's
num_miss_0$X

# <5> change_mou
summary(tel_data_1$change_mou) # 414 NA's
tel_data_1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->change_mou_1
change_mou_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(change_mou,n=10))%>%
                           count(dec)%>%unname())[[2]]
change_mou_1$churn_perc<-change_mou_1$n/change_mou_1$N
change_mou_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(change_mou,n=10))%>%
                                     group_by(dec)%>%summarise(min(change_mou)))[[2]]
change_mou_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(change_mou,n=10))%>%
                                  group_by(dec)%>%summarise(max(change_mou)))[[2]]
change_mou_1$varname<-rep("change_mou",nrow(change_mou_1))
change_mou_1 # NA churn_perc is not close to any decile
tel_data_1$change_mou[is.na(tel_data_1$change_mou)]<- -9.183 # imputing by mean
summary(tel_data_1$change_mou) # no NA's
num_miss_0$X

# <6> income
summary(tel_data_1$income) # 16528 NA's
tel_data_1%>%mutate(dec=ntile(income,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->income_1
income_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(income,n=8))%>%
                           count(dec)%>%unname())[[2]]
income_1$churn_perc<-income_1$n/income_1$N
income_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(income,n=8))%>%
                                     group_by(dec)%>%summarise(min(income)))[[2]]
income_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(income,n=8))%>%
                                  group_by(dec)%>%summarise(max(income)))[[2]]
income_1$varname<-rep("income",nrow(income_1))
income_1 # NA churn_perc is not close to any decile
tel_data_1$income[is.na(tel_data_1$income)]<- 5 # imputing by mean
summary(tel_data_1$income) # no NA's
num_miss_0$X

# <7> ovrrev_Mean
summary(tel_data_1$ovrrev_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->ovrrev_Mean_1
ovrrev_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%
                           count(dec)%>%unname())[[2]]
ovrrev_Mean_1$churn_perc<-ovrrev_Mean_1$n/ovrrev_Mean_1$N
ovrrev_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%
                                     group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
ovrrev_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(ovrrev_Mean,n=8))%>%
                                  group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
ovrrev_Mean_1$varname<-rep("ovrrev_Mean",nrow(ovrrev_Mean_1))
ovrrev_Mean_1 # NA churn_perc is not close to any decile
tel_data_1$ovrrev_Mean[is.na(tel_data_1$ovrrev_Mean)]<- 13.22 # imputing by mean
summary(tel_data_1$ovrrev_Mean) # no NA's
num_miss_0$X

# <8> rev_Mean
summary(tel_data_1$rev_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->rev_Mean_1
rev_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(rev_Mean,n=10))%>%
                           count(dec)%>%unname())[[2]]
rev_Mean_1$churn_perc<-rev_Mean_1$n/rev_Mean_1$N
rev_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(rev_Mean,n=10))%>%
                                     group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
rev_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(rev_Mean,n=10))%>%
                                  group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
rev_Mean_1$varname<-rep("rev_Mean",nrow(rev_Mean_1))
rev_Mean_1 # NA churn_perc is not close to any decile
tel_data_1$rev_Mean[is.na(tel_data_1$rev_Mean)]<- 59.080 # imputing with mean
summary(tel_data_1$rev_Mean) # no NA's
num_miss_0$X

# <9> ovrmou_Mean
summary(tel_data_1$ovrmou_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->ovrmou_Mean_1
ovrmou_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(ovrmou_Mean,n=8))%>%
                           count(dec)%>%unname())[[2]]
ovrmou_Mean_1$churn_perc<-ovrmou_Mean_1$n/ovrmou_Mean_1$N
ovrmou_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(ovrmou_Mean,n=8))%>%
                                     group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
ovrmou_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(ovrmou_Mean,n=8))%>%
                                  group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
ovrmou_Mean_1$varname<-rep("ovrmou_Mean",nrow(ovrmou_Mean_1))
ovrmou_Mean_1 # NA churn_perc of is not close to any decile
tel_data_1$ovrmou_Mean[is.na(tel_data_1$ovrmou_Mean)]<- 40.18 # imputing with mean
summary(tel_data_1$ovrmou_Mean) # no NA's
num_miss_0$X

# <10> avg6mou
summary(tel_data_1$avg6mou) # 2056 NA's
tel_data_1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->avg6mou_1
avg6mou_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(avg6mou,n=10))%>%
                           count(dec)%>%unname())[[2]]
avg6mou_1$churn_perc<-avg6mou_1$n/avg6mou_1$N
avg6mou_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(avg6mou,n=10))%>%
                                     group_by(dec)%>%summarise(min(avg6mou)))[[2]]
avg6mou_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(avg6mou,n=10))%>%
                                  group_by(dec)%>%summarise(max(avg6mou)))[[2]]
avg6mou_1$varname<-rep("avg6mou",nrow(avg6mou_1))
avg6mou_1 # NA churn_perc of 0.1746109 is not close to any decile
tel_data_1$avg6mou[is.na(tel_data_1$avg6mou)]<- 521.4 # imputing with mean
summary(tel_data_1$avg6mou) # no NA's
num_miss_0$X

# <11> avg6qty
summary(tel_data_1$avg6qty) # 2056 NA's
tel_data_1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->avg6qty_1
avg6qty_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(avg6qty,n=10))%>%
                           count(dec)%>%unname())[[2]]
avg6qty_1$churn_perc<-avg6qty_1$n/avg6qty_1$N
avg6qty_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(avg6qty,n=10))%>%
                                     group_by(dec)%>%summarise(min(avg6qty)))[[2]]
avg6qty_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(avg6qty,n=10))%>%
                                  group_by(dec)%>%summarise(max(avg6qty)))[[2]]
avg6qty_1$varname<-rep("avg6qty",nrow(avg6qty_1))
avg6qty_1 # NA churn_perc is not close to any decile
tel_data_1$avg6qty[is.na(tel_data_1$avg6qty)]<- 182.2 # imputing with mean
summary(tel_data_1$avg6qty) # no NA's
num_miss_0$X

# <12> age1
summary(tel_data_1$age1) # 1152 NA's
tel_data_1%>%mutate(dec=ntile(age1,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->age1_1
age1_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(age1,n=9))%>%
                           count(dec)%>%unname())[[2]]
age1_1$churn_perc<-age1_1$n/age1_1$N
age1_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(age1,n=9))%>%
                                     group_by(dec)%>%summarise(min(age1)))[[2]]
age1_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(age1,n=9))%>%
                                  group_by(dec)%>%summarise(max(age1)))[[2]]
age1_1$varname<-rep("age1",nrow(age1_1))
age1_1 # NA churn_perc is not close to any decile
tel_data_1$age1[is.na(tel_data_1$age1)]<- 32 # imputing with mean
summary(tel_data_1$age1) # no NA's
num_miss_0$X

# <13> age2
summary(tel_data_1$age2) # 1152 NA's
tel_data_1%>%mutate(dec=ntile(age2,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->age2_1
age2_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(age2,n=7))%>%
                           count(dec)%>%unname())[[2]]
age2_1$churn_perc<-age2_1$n/age2_1$N
age2_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(age2,n=7))%>%
                                     group_by(dec)%>%summarise(min(age2)))[[2]]
age2_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(age2,n=7))%>%
                                  group_by(dec)%>%summarise(max(age2)))[[2]]
age2_1$varname<-rep("age2",nrow(age2_1))
age2_1 # NA churn_perc is not close to any decile
tel_data_1$age2[is.na(tel_data_1$age2)]<- 21 # imputing with mean
summary(tel_data_1$age1) # no NA's
num_miss_0$X

# <14> hnd_price
summary(tel_data_1$hnd_price) # 636 NA's
tel_data_1%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->hnd_price_1
hnd_price_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(hnd_price,n=10))%>%
                           count(dec)%>%unname())[[2]]
hnd_price_1$churn_perc<-hnd_price_1$n/hnd_price_1$N
hnd_price_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(hnd_price,n=10))%>%
                                     group_by(dec)%>%summarise(min(hnd_price)))[[2]]
hnd_price_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(hnd_price,n=10))%>%
                                  group_by(dec)%>%summarise(max(hnd_price)))[[2]]
hnd_price_1$varname<-rep("hnd_price",nrow(hnd_price_1))
hnd_price_1 # NA churn_perc is not close to any decile
tel_data_1$hnd_price[is.na(tel_data_1$hnd_price)]<-  105.20 # imputing with mean
summary(tel_data_1$hnd_price) # no NA's  
num_miss_0$X

# <15> forgntvl
summary(tel_data_1$forgntvl) # 1152 NA's
tel_data_1%>%mutate(dec=ntile(forgntvl,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->forgntvl_1
forgntvl_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                           count(dec)%>%unname())[[2]]
forgntvl_1$churn_perc<-forgntvl_1$n/forgntvl_1$N
forgntvl_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                                     group_by(dec)%>%summarise(min(forgntvl)))[[2]]
forgntvl_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                                  group_by(dec)%>%summarise(max(forgntvl)))[[2]]
forgntvl_1$varname<-rep("forgntvl",nrow(forgntvl_1))
forgntvl_1 # NA churn_perc is not close to any decile
tel_data_1$forgntvl[is.na(tel_data_1$forgntvl)]<-  0.0584 # imputing with mean
summary(tel_data_1$forgntvl) # no NA's  
num_miss_0$X

# <16> forgntvl
summary(tel_data_1$forgntvl) # 1152 NA's
tel_data_1%>%mutate(dec=ntile(forgntvl,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->forgntvl_1
forgntvl_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                           count(dec)%>%unname())[[2]]
forgntvl_1$churn_perc<-forgntvl_1$n/forgntvl_1$N
forgntvl_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                                     group_by(dec)%>%summarise(min(forgntvl)))[[2]]
forgntvl_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                                  group_by(dec)%>%summarise(max(forgntvl)))[[2]]
forgntvl_1$varname<-rep("forgntvl",nrow(forgntvl_1))
forgntvl_1 # NA churn_perc is not close to any decile
tel_data_1$forgntvl[is.na(tel_data_1$forgntvl)]<-  0.0584 # imputing with mean
summary(tel_data_1$forgntvl) # no NA's 
num_miss_0$X

# <16> mtrcycle
summary(tel_data_1$mtrcycle) # 1152 NA's
tel_data_1%>%mutate(dec=ntile(mtrcycle,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->mtrcycle_1
mtrcycle_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(mtrcycle,n=4))%>%
                           count(dec)%>%unname())[[2]]
mtrcycle_1$churn_perc<-mtrcycle_1$n/mtrcycle_1$N
mtrcycle_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(mtrcycle,n=4))%>%
                                     group_by(dec)%>%summarise(min(mtrcycle)))[[2]]
mtrcycle_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(mtrcycle,n=4))%>%
                                  group_by(dec)%>%summarise(max(mtrcycle)))[[2]]
mtrcycle_1$varname<-rep("mtrcycle",nrow(mtrcycle_1))
mtrcycle_1 # NA churn_perc is not close to any decile
tel_data_1$mtrcycle[is.na(tel_data_1$mtrcycle)]<-  0.0134 # imputing with mean
summary(tel_data_1$mtrcycle) # no NA's 
num_miss_0$X

# <17> truck
summary(tel_data_1$truck) # 1152 NA's
tel_data_1%>%mutate(dec=ntile(truck,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->truck_1
truck_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(truck,n=3))%>%
                           count(dec)%>%unname())[[2]]
truck_1$churn_perc<-truck_1$n/truck_1$N
truck_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(truck,n=3))%>%
                                     group_by(dec)%>%summarise(min(truck)))[[2]]
truck_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(truck,n=3))%>%
                                  group_by(dec)%>%summarise(max(truck)))[[2]]
truck_1$varname<-rep("truck",nrow(truck_1))
truck_1 # NA churn_perc is not close to any decile
tel_data_1$truck[is.na(tel_data_1$truck)]<-  0.1898 # imputing with mean
summary(tel_data_1$truck) # no NA's 
num_miss_0$X

# <18> roam_Mean
summary(tel_data_1$roam_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->roam_Mean_1
roam_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(roam_Mean,n=6))%>%
                           count(dec)%>%unname())[[2]]
roam_Mean_1$churn_perc<-roam_Mean_1$n/roam_Mean_1$N
roam_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(roam_Mean,n=6))%>%
                                     group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
roam_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(roam_Mean,n=6))%>%
                                  group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
roam_Mean_1$varname<-rep("roam_Mean",nrow(roam_Mean_1))
roam_Mean_1 # NA churn_perc is not close to any decile
tel_data_1$roam_Mean[is.na(tel_data_1$roam_Mean)]<-  1.261 # imputing with mean
summary(tel_data_1$roam_Mean) # no NA's 
num_miss_0$X

# <19> da_Mean
summary(tel_data_1$da_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->da_Mean_1
da_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(da_Mean,n=8))%>%
                           count(dec)%>%unname())[[2]]
da_Mean_1$churn_perc<-da_Mean_1$n/da_Mean_1$N
da_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(da_Mean,n=8))%>%
                                     group_by(dec)%>%summarise(min(da_Mean)))[[2]]
da_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(da_Mean,n=8))%>%
                                  group_by(dec)%>%summarise(max(da_Mean)))[[2]]
da_Mean_1$varname<-rep("da_Mean",nrow(da_Mean_1))
da_Mean_1 # NA churn_perc is not close to any decile
tel_data_1$da_Mean[is.na(tel_data_1$da_Mean)]<-  0.9042 # imputing with mean
summary(tel_data_1$da_Mean) # no NA's 
num_miss_0$X

# <20> da_Range
summary(tel_data_1$da_Range) # 181 NA's
tel_data_1%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->da_Range_1
da_Range_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(da_Range,n=8))%>%
                           count(dec)%>%unname())[[2]]
da_Range_1$churn_perc<-da_Range_1$n/da_Range_1$N
da_Range_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(da_Range,n=8))%>%
                                     group_by(dec)%>%summarise(min(da_Range)))[[2]]
da_Range_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(da_Range,n=8))%>%
                                  group_by(dec)%>%summarise(max(da_Range)))[[2]]
da_Range_1$varname<-rep("da_Range",nrow(da_Range_1))
da_Range_1 # NA churn_perc is not close to any decile
tel_data_1$da_Range[is.na(tel_data_1$da_Range)]<-  1.645 # imputing with mean
summary(tel_data_1$da_Range) # no NA's
num_miss_0$X

# <21> datovr_Mean
summary(tel_data_1$datovr_Mean) # 181 NA's
tel_data_1%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->datovr_Mean_1
datovr_Mean_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(datovr_Mean,n=5))%>%
                           count(dec)%>%unname())[[2]]
datovr_Mean_1$churn_perc<-datovr_Mean_1$n/datovr_Mean_1$N
datovr_Mean_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(datovr_Mean,n=5))%>%
                                     group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
datovr_Mean_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(datovr_Mean,n=5))%>%
                                  group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
datovr_Mean_1$varname<-rep("datovr_Mean",nrow(datovr_Mean_1))
datovr_Mean_1 # NA churn_perc is not close to any decile
tel_data_1$datovr_Mean[is.na(tel_data_1$datovr_Mean)]<-  0.2541 # imputing with mean
summary(tel_data_1$datovr_Mean) # no NA's
num_miss_0$X

# <22> datovr_Range
summary(tel_data_1$datovr_Range) # 181 NA's
tel_data_1%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->datovr_Range_1
datovr_Range_1$N<-unclass(tel_data_1%>%mutate(dec=ntile(datovr_Range,n=5))%>%
                           count(dec)%>%unname())[[2]]
datovr_Range_1$churn_perc<-datovr_Range_1$n/datovr_Range_1$N
datovr_Range_1$GreaterThan<-unclass(tel_data_1%>%mutate(dec=ntile(datovr_Range,n=5))%>%
                                     group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
datovr_Range_1$LessThan<-unclass(tel_data_1%>%mutate(dec=ntile(datovr_Range,n=5))%>%
                                  group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
datovr_Range_1$varname<-rep("datovr_Range",nrow(datovr_Range_1))
datovr_Range_1 # NA churn_perc is not close to any decile
tel_data_1$datovr_Range[is.na(tel_data_1$datovr_Range)]<-  0.7277 # imputing with mean
summary(tel_data_1$datovr_Range) # no NA's

# <23> models - variable with 1 NA only 
summary(tel_data_1$models)
tel_data_1$models[is.na(tel_data_1$models)]<-  1.568 # imputing with mean
summary(tel_data_1$models) # no NA's

# <24> eqpdays - variable with 1 NA only 
summary(tel_data_1$eqpdays)
tel_data_1$eqpdays[is.na(tel_data_1$eqpdays)]<-  377.1 # imputing with mean
summary(tel_data_1$eqpdays) # no NA's

# categorical variable missing value treatement: 7 variables + 1 variable with 1 NA 
# <a> prizm_social_one
summary(tel_data_1$prizm_social_one)
tel_data_1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->prizm_social_one_1
prizm_social_one_1$N<-unclass(tel_data_1%>%filter(
  prizm_social_one%in%prizm_social_one_1$levels)%>%count(prizm_social_one))[[2]]
prizm_social_one_1$ChurnPerc<-prizm_social_one_1$n/prizm_social_one_1$N
prizm_social_one_1$Var.Name<-rep("prizm_social_one",nrow(prizm_social_one_1))
prizm_social_one_1 # NA ChurnPerc is close to factor T 
tel_data_1$prizm_social_one[is.na(tel_data_1$prizm_social_one)]<-  "T"
summary(tel_data_1$prizm_social_one) # no NA's
cat_miss_0$X

# <b> area
summary(tel_data_1$area) # 18 NAs
tel_data_1%>%count(churn,levels=area)%>%filter(churn==1)->area_1
area_1$N<-unclass(tel_data_1%>%filter(
  area%in%area_1$levels)%>%count(area))[[2]]
area_1$ChurnPerc<-area_1$n/area_1$N
area_1$Var.Name<-rep("area",nrow(area_1))
area_1 # NA ChurnPerc is close to factor HOUSTON AREA
tel_data_1$area[is.na(tel_data_1$area)]<- "HOUSTON AREA"
summary(tel_data_1$area) # no NA's
cat_miss_0$X

# <c> hnd_webcap
summary(tel_data_1$hnd_webcap) #6063 NAs
tel_data_1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->hnd_webcap_1
hnd_webcap_1$N<-unclass(tel_data_1%>%filter(
  hnd_webcap%in%hnd_webcap_1$levels)%>%count(hnd_webcap))[[2]]
hnd_webcap_1$ChurnPerc<-hnd_webcap_1$n/hnd_webcap_1$N
hnd_webcap_1$Var.Name<-rep("hnd_webcap",nrow(hnd_webcap_1))
hnd_webcap_1 # NA ChurnPerc is close to factor WC
tel_data_1$hnd_webcap[is.na(tel_data_1$hnd_webcap)]<- "WC"
summary(tel_data_1$hnd_webcap) # no NA's
cat_miss_0$X

# <d> marital
summary(tel_data_1$marital) #1152 NAs
tel_data_1%>%count(churn,levels=marital)%>%filter(churn==1)->marital_1
marital_1$N<-unclass(tel_data_1%>%filter(
  marital%in%marital_1$levels)%>%count(marital))[[2]]
marital_1$ChurnPerc<-marital_1$n/marital_1$N
marital_1$Var.Name<-rep("marital",nrow(marital_1))
marital_1 # NA ChurnPerc is close to factor S
tel_data_1$marital[is.na(tel_data_1$marital)]<- "S"
summary(tel_data_1$marital) # no NA's
cat_miss_0$X

# <e> ethnic
summary(tel_data_1$ethnic) #1152 NAs
tel_data_1%>%count(churn,levels=ethnic)%>%filter(churn==1)->ethnic_1
ethnic_1$N<-unclass(tel_data_1%>%filter(
  ethnic%in%ethnic_1$levels)%>%count(ethnic))[[2]]
ethnic_1$ChurnPerc<-ethnic_1$n/ethnic_1$N
ethnic_1$Var.Name<-rep("ethnic",nrow(ethnic_1))
ethnic_1 # NA ChurnPerc is close to factor M
tel_data_1$ethnic[is.na(tel_data_1$ethnic)]<- "M"
summary(tel_data_1$ethnic) # no NA's
cat_miss_0$X

# <f> car_buy
summary(tel_data_1$car_buy) #1152 NAs
tel_data_1%>%count(churn,levels=car_buy)%>%filter(churn==1)->car_buy_1
car_buy_1$N<-unclass(tel_data_1%>%filter(
  car_buy%in%car_buy_1$levels)%>%count(car_buy))[[2]]
car_buy_1$ChurnPerc<-car_buy_1$n/car_buy_1$N
car_buy_1$Var.Name<-rep("car_buy",nrow(car_buy_1))
car_buy_1 # NAs are imputed to UNKNOWN - Assumption
tel_data_1$car_buy[is.na(tel_data_1$car_buy)]<- "UNKNOWN"
summary(tel_data_1$car_buy) # no NA's
cat_miss_0$X

# <g> csa
summary(tel_data_1$csa) #18 NAs
tel_data_1%>%count(churn,levels=csa)%>%filter(churn==1)->csa_1
csa_1$N<-unclass(tel_data_1%>%filter(
  csa%in%csa_1$levels)%>%count(csa))[[2]]
csa_1$ChurnPerc<-csa_1$n/csa_1$N
csa_1$Var.Name<-rep("csa",nrow(csa_1))
csa_1 # # NAs are imputed to AIRCOL803 - Assumption
tel_data_1$csa[is.na(tel_data_1$csa)]<- "AIRCOL803"
summary(tel_data_1$csa) # no NA's
str(tel_data_1$csa)

# <h> refurb_new - variable with 1 NA only 
summary(tel_data_1$refurb_new)
tel_data_1$refurb_new[is.na(tel_data_1$refurb_new)]<- "N" # imputing with mmax occurence
summary(tel_data_1$refurb_new) # no NA's

## Optimise check the updated dataset tel_data_1---------------------------------
# create updated dataset tel_data_1 QR
checkDataQuality(tel_data_1, "up_num.csv", "up_cat.csv")
up_num=read.csv("up_num.csv") # continuous variable quality report/dataframe
nrow(up_num) # 58 continuous variables 
View(up_num)
up_cat=read.csv("up_cat.csv") # categorical variable quality report/dataframe
nrow(up_cat) # 10 categorical variables
View(up_cat)
up_cat$X
# remove redundant, high unique values categorical variables
tel_data_1 = select(tel_data_1,-c(crclscod, csa))
# remove Customer_ID
tel_data_1 = select(tel_data_1, -Customer_ID)
ncol(tel_data_1) # updated dataset tel_data1 have 66 variables

# check - updated dataset tel_data_1 do not have missing values
colSums(is.na(tel_data_1))
sum(colSums(is.na(tel_data_1)))

## Create Logistic (GLM) Model---------------------------------------------------
# split data into training and test set
set.seed(200)
index<-sample(nrow(tel_data_1), 0.70*nrow(tel_data_1), replace=F)
train_set = tel_data_1[index,]
test_set = tel_data_1[-index,]
# check distribution
nrow(test_set)/(nrow(train_set)+nrow(test_set)) # check % distribution
table(tel_data_1$churn)/nrow(tel_data_1) # check churn distribution
table(train_set$churn)/nrow(train_set)
table(test_set$churn)/nrow(test_set)

# Create first model-----
mod0<-glm(train_set$churn~., data=train_set, family="binomial")
summ<-summary(mod0)
summ
# significant variables from first model run:
summ_df<-as.data.frame(summ$coeff)
nrow(summ_df)
summ_df_sig<-summ_df[summ_df$`Pr(>|z|)`<0.05,]
nrow(summ_df_sig)

# continuous variables:
#totmrc_Mean+mou_Range+change_mou+months+eqpdays+rev_Mean+avgmou+age1+models+...
#...hnd_price+actvsubs+uniqsubs+drop_blk_Mean+custcare_Mean+iwylis_vce_Mean+...
#...adjqty+comp_vce_Mean+truck+mou_pead_Mean+drop_vce_Mean+avg3mou+adjmou+retdays_up

cont_var_0<-c("totmrc_Mean","mou_Range","change_mou","months","eqpdays","rev_Mean",
              "avgmou","age1","models","hnd_price","actvsubs","uniqsubs",
              "drop_blk_Mean","custcare_Mean","iwylis_vce_Mean","adjqty",
              "comp_vce_Mean","truck","mou_pead_Mean","drop_vce_Mean",
              "avg3mou","adjmou", "retdays_up")
cont_var_0 # count=> 23

# categorical variables:
#asl_flagY+prizm_social_oneR+areaNORTHWEST/ROCKY MOUNTA+areaSOUTH FLORIDA AREA+...
#...refurb_newR+ethnicC+ethnicZ+prizm_social_oneT+ethnicF+ethnicG+ethnicH+...
#...ethnicJ+ethnicM+ethnicP+ethnicU+areaNEW YORK CITY AREA+areaPHILADELPHIA AREA+...
#...ethnicN+ethnicS

cat_var_0<-c("asl_flagY","prizm_social_oneR","areaNORTHWEST/ROCKY MOUNTA",
             "areaSOUTH FLORIDA AREA","refurb_newR","ethnicC","ethnicZ",
             "prizm_social_oneT","ethnicF","ethnicG","ethnicH","ethnicJ",
             "ethnicM","ethnicP","ethnicU","areaNEW YORK CITY AREA",
             "areaPHILADELPHIA AREA","ethnicN","ethnicS")
cat_var_0 # count=> 19

# create dummy variable for the significant categorical variables
#asl_flag Y
summary(tel_data_1$asl_flag)
train_set$asl_flag_Y<-ifelse(train_set$asl_flag == "Y", 1, 0)
test_set$asl_flag_Y<-ifelse(test_set$asl_flag == "Y", 1, 0)
#prizm_social_one R
summary(tel_data_1$prizm_social_one)
train_set$prizm_social_one_R<-ifelse(train_set$prizm_social_one == "R", 1, 0)
test_set$prizm_social_one_R<-ifelse(test_set$prizm_social_one == "R", 1, 0)
#prizm_social_one T
train_set$prizm_social_one_T<-ifelse(train_set$prizm_social_one == "T", 1, 0)
test_set$prizm_social_one_T<-ifelse(test_set$prizm_social_one == "T", 1, 0)
#area NORTHWEST/ROCKY MOUNTA
summary(tel_data_1$area)
train_set$area_rocky<-ifelse(train_set$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)
test_set$area_rocky<-ifelse(test_set$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)
#area SOUTH FLORIDA AREA
train_set$area_florida<-ifelse(train_set$area == "SOUTH FLORIDA AREA", 1, 0)
test_set$area_florida<-ifelse(test_set$area == "SOUTH FLORIDA AREA", 1, 0)
#area NEW YORK CITY AREA
train_set$area_newyork<-ifelse(train_set$area == "NEW YORK CITY AREA", 1, 0)
test_set$area_newyork<-ifelse(test_set$area == "NEW YORK CITY AREA", 1, 0)
#area PHILADELPHIA AREA
train_set$area_phila<-ifelse(train_set$area == "PHILADELPHIA AREA", 1, 0)
test_set$area_phila<-ifelse(test_set$area == "PHILADELPHIA AREA", 1, 0)
#refurb_new R
summary(tel_data_1$refurb_new)
train_set$refurb_new_R<-ifelse(train_set$refurb_new == "R", 1, 0)
test_set$refurb_new_R<-ifelse(test_set$refurb_new == "R", 1, 0)
#ethnic C
summary(tel_data_1$ethnic)
train_set$ethnic_C<-ifelse(train_set$ethnic == "C", 1, 0)
test_set$ethnic_C<-ifelse(test_set$ethnic == "C", 1, 0)
#ethnic Z
train_set$ethnic_Z<-ifelse(train_set$ethnic == "Z", 1, 0)
test_set$ethnic_Z<-ifelse(test_set$ethnic == "Z", 1, 0)
#ethnic F
train_set$ethnic_F<-ifelse(train_set$ethnic == "F", 1, 0)
test_set$ethnic_F<-ifelse(test_set$ethnic == "F", 1, 0)
#ethnic G
train_set$ethnic_G<-ifelse(train_set$ethnic == "G", 1, 0)
test_set$ethnic_G<-ifelse(test_set$ethnic == "G", 1, 0)
#ethnic H
train_set$ethnic_H<-ifelse(train_set$ethnic == "H", 1, 0)
test_set$ethnic_H<-ifelse(test_set$ethnic == "H", 1, 0)
#ethnic J
train_set$ethnic_J<-ifelse(train_set$ethnic == "J", 1, 0)
test_set$ethnic_J<-ifelse(test_set$ethnic == "J", 1, 0)
#ethnic M
train_set$ethnic_M<-ifelse(train_set$ethnic == "M", 1, 0)
test_set$ethnic_M<-ifelse(test_set$ethnic == "M", 1, 0)
#ethnic P
train_set$ethnic_P<-ifelse(train_set$ethnic == "P", 1, 0)
test_set$ethnic_P<-ifelse(test_set$ethnic == "P", 1, 0)
#ethnic U
train_set$ethnic_U<-ifelse(train_set$ethnic == "U", 1, 0)
test_set$ethnic_U<-ifelse(test_set$ethnic == "U", 1, 0)
#ethnic N
train_set$ethnic_N<-ifelse(train_set$ethnic == "N", 1, 0)
test_set$ethnic_N<-ifelse(test_set$ethnic == "N", 1, 0)
#ethnic S
train_set$ethnic_S<-ifelse(train_set$ethnic == "S", 1, 0)
test_set$ethnic_S<-ifelse(test_set$ethnic == "S", 1, 0)

# asl_flag_Y+prizm_social_one_R+prizm_social_one_T+area_rocky+area_florida+...
#...area_newyork+area_phila+refurb_new_R+ethnic_C+ethnic_Z+ethnic_F+ethnic_G...
#...+ethnic_H+ethnic_J+ethnic_M+ethnic_P+ethnic_U+ethnic_N+ethnic_S

# Create second model using significant variables only---------------------------
mod1<-glm(train_set$churn~totmrc_Mean+mou_Range+change_mou+months+eqpdays+
            rev_Mean+avgmou+age1+models+hnd_price+actvsubs+uniqsubs+
            drop_blk_Mean+custcare_Mean+iwylis_vce_Mean+adjqty+comp_vce_Mean+
            truck+mou_pead_Mean+drop_vce_Mean+avg3mou+adjmou+retdays_up+
            asl_flag_Y+prizm_social_one_R+prizm_social_one_T+area_rocky+
            area_florida+area_newyork+area_phila+refurb_new_R+ethnic_C+
            ethnic_Z+ethnic_F+ethnic_G+ethnic_H+ethnic_J+ethnic_M+ethnic_P+
            ethnic_U+ethnic_N+ethnic_S, data=train_set, family="binomial")
summ<-summary(mod1)
summ
# significant variables from first model run:
summ_df<-as.data.frame(summ$coeff)
nrow(summ_df)
summ_df_sig<-summ_df[summ_df$`Pr(>|z|)`<0.05,]
nrow(summ_df_sig)
# 5 out of 42 variables came insignificant in second run:
# => iwylis_vce_Mean, adjqty, comp_vce_Mean, truck, mou_pead_Mean

# Create third model removing above five insignificant variables-----------------
mod2<-glm(train_set$churn~totmrc_Mean+mou_Range+change_mou+months+eqpdays+
            rev_Mean+avgmou+age1+models+hnd_price+actvsubs+uniqsubs+
            drop_blk_Mean+custcare_Mean+drop_vce_Mean+avg3mou+adjmou+retdays_up+
            asl_flag_Y+prizm_social_one_R+prizm_social_one_T+area_rocky+
            area_florida+area_newyork+area_phila+refurb_new_R+ethnic_C+
            ethnic_Z+ethnic_F+ethnic_G+ethnic_H+ethnic_J+ethnic_M+ethnic_P+
            ethnic_U+ethnic_N+ethnic_S, data=train_set, family="binomial")
summ<-summary(mod2)
summ
# significant variables from first model run:
summ_df<-as.data.frame(summ$coeff)
nrow(summ_df)
summ_df_sig<-summ_df[summ_df$`Pr(>|z|)`<0.05,]
nrow(summ_df_sig)
# variable "adjmou" came insignificant in third run

# Create fourth model removing "adjmou" insignificant variables------------------
mod3<-glm(train_set$churn~totmrc_Mean+mou_Range+change_mou+months+eqpdays+
            rev_Mean+avgmou+age1+models+hnd_price+actvsubs+uniqsubs+
            drop_blk_Mean+custcare_Mean+drop_vce_Mean+avg3mou+retdays_up+
            asl_flag_Y+prizm_social_one_R+prizm_social_one_T+area_rocky+
            area_florida+area_newyork+area_phila+refurb_new_R+ethnic_C+
            ethnic_Z+ethnic_F+ethnic_G+ethnic_H+ethnic_J+ethnic_M+ethnic_P+
            ethnic_U+ethnic_N+ethnic_S, data=train_set, family="binomial")
summ<-summary(mod3)
summ
# significant variables from first model run:
summ_df<-as.data.frame(summ$coeff)
nrow(summ_df)
summ_df_sig<-summ_df[summ_df$`Pr(>|z|)`<0.05,]
nrow(summ_df_sig)
# all variable found significant in fourth run

# Create fifth model removing variables with vif greater than 5------------------
# Check for Multicollinearity
library(car)
col_chk<-vif(mod3)
col_chk
# find variable with vif greater than 5
col_chk_df<-as.data.frame(col_chk)
col_chk_df
nrow(col_chk_df)
col_chk_df_g5<-col_chk_df[col_chk_df$col_chk > 5,]
nrow(col_chk_df_g5)
col_chk_df_g5
# variable "avgmou" and "avg3mou" found with vif greater than 5
# create fifth model removing above two high vif variables
mod4<-glm(train_set$churn~totmrc_Mean+mou_Range+change_mou+months+eqpdays+
            rev_Mean+age1+models+hnd_price+actvsubs+uniqsubs+
            drop_blk_Mean+custcare_Mean+drop_vce_Mean+retdays_up+
            asl_flag_Y+prizm_social_one_R+prizm_social_one_T+area_rocky+
            area_florida+area_newyork+area_phila+refurb_new_R+ethnic_C+
            ethnic_Z+ethnic_F+ethnic_G+ethnic_H+ethnic_J+ethnic_M+ethnic_P+
            ethnic_U+ethnic_N+ethnic_S, data=train_set, family="binomial")
summ<-summary(mod4)
summ
# significant variables from first model run:
summ_df<-as.data.frame(summ$coeff)
nrow(summ_df)
summ_df_sig<-summ_df[summ_df$`Pr(>|z|)`<0.05,]
nrow(summ_df_sig)
# two variables "drop_blk_Mean" and "drop_vce_Mean" came insignificant in fourth run

# Create sixth model removing above two insignificant variables------------------
mod5<-glm(train_set$churn~totmrc_Mean+mou_Range+change_mou+months+eqpdays+
            rev_Mean+age1+models+hnd_price+actvsubs+uniqsubs+
            custcare_Mean+retdays_up+
            asl_flag_Y+prizm_social_one_R+prizm_social_one_T+area_rocky+
            area_florida+area_newyork+area_phila+refurb_new_R+ethnic_C+
            ethnic_Z+ethnic_F+ethnic_G+ethnic_H+ethnic_J+ethnic_M+ethnic_P+
            ethnic_U+ethnic_N+ethnic_S, data=train_set, family="binomial")
summ<-summary(mod5)
summ
# significant variables from first model run:
summ_df<-as.data.frame(summ$coeff)
nrow(summ_df)
summ_df_sig<-summ_df[summ_df$`Pr(>|z|)`<0.05,]
nrow(summ_df_sig)
# all variable found significant in sixth run

# Check for Multicollinearity
col_chk<-vif(mod5)
col_chk
# find variable with vif greater than 5
col_chk_df<-as.data.frame(col_chk)
col_chk_df
nrow(col_chk_df)
col_chk_df_g5<-col_chk_df[col_chk_df$col_chk > 5,]
nrow(col_chk_df_g5)
col_chk_df_g5
# all variable found with vif less than 5 in sixth run
# therefore sixth model (mod5) can be finalised for testing

## Model testing-----------------------------------------------------------------
# prediction - probabilty
pred<-predict(mod5, type="response", newdata = test_set) # probability
head(pred)
# prediction - 1, 0
table(tel_data_1$churn)/nrow(tel_data_1) # churn distribution complete dataset
churn_ratio<-0.2392114
pred1<-ifelse(pred>=churn_ratio, 1, 0) # 1, 0 value
table(pred1)
head(pred1)

# kappa matrix
library(irr)
kappa2(data.frame(test_set$churn, pred1))

# confusion matrix
library(caret)
confusionMatrix(pred1, test_set$churn, positive="1")

# ROCR curve
library(ROCR)
pred2<-prediction(pred, test_set$churn)
pref<-performance(pred2, "tpr", "fpr")
plot(pref, col="red")
abline(0,1, lty=8, col="grey")
auc<-performance(pred2, "auc")
auc

# Gain chart
library(gains)
gains(test_set$churn, pred, groups = 10)

# probability distribution
test_set$prob<-pred
quantile(test_set$prob, prob=c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1))











































