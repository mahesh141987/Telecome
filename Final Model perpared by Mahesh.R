## Runing library
library(dplyr)
library(gains)
library(car)
library(stats)
library(Hmisc)
library(readr)
library(data.table)
library(ggplot2)

rm(list = ls())
setwd("C:\\Jig19406\\Final model")

telecomfinal <- read_csv("telecomfinal.csv")
te=telecomfinal
tele=telecomfinal
colSums(is.na(te))
options(scipen = 999)##To prevent scientific notation

#Creating a data quality report should be done before raw data which we have recived 
## cheking  VariableName and data type
VariableName= names(tele)
q1=as.data.frame(VariableName)
rm(VariableName)
q1$DataType=sapply(tele,class)
## find in No Of Records & UniqueRecords
q1$NoOfRecords=nrow(tele)

for(i in 1 :ncol(tele))
{ q1$UniqueRecords [i]<-length(unique(tele[,i]))
}

## no of DataAvailable	and AvailablePercent
q1$DataAvailable=colSums(!is.na(tele)) ## !is.na asking for count non NA values
q1$AvailablePercent=round(colMeans(!is.na(tele)),3)## if we take two decimal more number is getting around to 1 so 3digit isshowning optimum

#Missing	MissingPercent
q1$Missing=colSums(is.na(tele))
q1$MissingPercent=round(colMeans(is.na(tele)),3)

#Minimum	Maximum	Mean and 5th Percentile	10th Percentile	25th Percentile	50th Percentile	75th Percentile	90th Percentile	95th Percentile


for (i in 1:ncol(tele)) {
  
  q1$Minimum[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",min(tele[,i],na.rm=T),0),2)
  q1$Maximun[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",max(tele[,i],na.rm=T),0),2)
  q1$Mean[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",mean(tele[,i],na.rm=T),0),2)
  q1$Percentile_5th[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.05,na.rm=T),0),2)
  q1$Percentile_10th[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.10,na.rm=T),0),2)
  q1$Percentile_25th[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.25,na.rm=T),0),2)
  q1$Percentile_50th[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.50,na.rm=T),0),2)
  q1$Percentile_75th[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.75,na.rm=T),0),2)
  q1$Percentile_90th[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.90,na.rm=T),0),2)
  q1$Percentile_95th[i]=round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.95,na.rm=T),0),2)
  
  
}

str(q1)
getwd()
write.csv(q1," Data Quality report.csv",row.names = T)

rm(q1)


##Creating a data quality report
#mou_Mean
te%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d12
d12$N<-unclass(te%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]] 
d12$churn_perc<-d12$n/d12$N
d12$varname<-rep("mou_Mean",nrow(d12))
hist(d12$n)
ggplot(d12,aes(x=d12$dec,y=d12$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#numbcars it has 6 part remove numbcars
te%>%mutate(dec=ntile(numbcars,n=10))%>%count(churn,dec)%>%filter(churn==1)->d2
hist(d2$n)

#adjrev
te%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->d3
d3$N<-unclass(te%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]] 
d3$churn_perc<-d3$n/d3$N
d3$varname<-rep("adjrev",nrow(d3))
hist(d3$n)
ggplot(d3,aes(x=d3$dec,y=d3$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#avg6mou has 11 Part treatment of missing will take it 
te%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->d4
d4$N<-unclass(te%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]] 
d4$churn_perc<-d4$n/d4$N
d4$varname<-rep("avg6mou",nrow(d4))
ggplot(d4,aes(x=d4$dec,y=d4$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc
hist(d4$n)
#age1 10 part is NA to create dummy as age can be claissified 
te%>%mutate(dec=ntile(age1,n=10))%>%count(churn,dec)%>%filter(churn==1)->d5
d5$N<-unclass(te%>%mutate(dec=ntile(age1,n=10))%>%count(dec)%>%unname())[[2]] 
hist(d5$n)

#age2 part has 6  partcreate dummy as age can be claissified 
te%>%mutate(dec=ntile(age2,n=10))%>%count(churn,dec)%>%filter(churn==1)->d6
hist(d6$n)

#avgrev
te%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->d7
d7$N<-unclass(te%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]] 
d7$churn_perc<-d7$n/d7$N
d7$varname<-rep("avgrev",nrow(d7))
hist(d7$n)
ggplot(d7,aes(x=d7$dec,y=d7$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#hnd_price we can use this variable are numeric or factor
te%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->d10
d10$N<-unclass(te%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]] 
d10$churn_perc<-d10$n/d10$N
d10$varname<-rep("hnd_price",nrow(d10))
hist(d10$n)
ggplot(d10,aes(x=d10$dec,y=d10$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#change_mou
te%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->d11
d11$N<-unclass(te%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]] 
d11$churn_perc<-d11$n/d11$N
d11$varname<-rep("change_mou",nrow(d11))
hist(d11$n)
ggplot(d11,aes(x=d11$dec,y=d11$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc



#mou_Range
te%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d13
d13$N<-unclass(te%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]] 
d13$churn_perc<-d13$n/d13$N
d13$varname<-rep("mou_Range",nrow(d13))
hist(d13$n)
ggplot(d13,aes(x=d13$dec,y=d13$churn_perc))+geom_point(color='red')+geom_smooth()

#ovrrev_Mean If model is not runing but it is revenue related so we will check in level significate and need to remove or not will finalised  
te%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d14
hist(d14$n)
ggplot(d14,aes(x=d14$dec,y=d14$churn_perc))+geom_point(color='red')+geom_smooth()

##rev_Mean
te%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d15
d15$N<-unclass(te%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]] 
d15$churn_perc<-d15$n/d15$N
d15$varname<-rep("rev_Mean",nrow(d15))
hist(d15$n)
ggplot(d15,aes(x=d15$dec,y=d15$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#roam_Mean  remove it as less than dec if model is not working
te%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d16
hist(d16$n)

#da_Mean less than  10  
te%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d17
hist(d17$n)

#da_Range less than 10
te%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d18
hist(d18$n)

#datovr_Range less than 10
te%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d20
hist(d20$n)



#drop_blk_Mean
te%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d23
d23$N<-unclass(te%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
d23$churn_perc<-d23$n/d23$N
d23$varname<-rep("drop_blk_Mean",nrow(d23))
hist(d23$n)
ggplot(d23,aes(x=d23$dec,y=d23$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#drop_vce_Range
te%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d24
d24$N<-unclass(te%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
d24$churn_perc<-d24$n/d24$N
d24$varname<-rep("drop_vce_Range",nrow(d24))
hist(d24$n)
ggplot(d24,aes(x=d24$dec,y=d24$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#owylis_vce_Range
te%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d25
d25$N<-unclass(te%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
d25$churn_perc<-d25$n/d25$N
d25$varname<-rep("owylis_vce_Range",nrow(d25))
hist(d25$n)
ggplot(d25,aes(x=d25$dec,y=d25$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#mou_opkv_Range
te%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d26
d26$N<-unclass(te%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
d26$churn_perc<-d26$n/d26$N
d26$varname<-rep("mou_opkv_Range",nrow(d26))
hist(d26$n)
ggplot(d26,aes(x=d26$dec,y=d26$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#months
te%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->d27
d27$N<-unclass(te%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
d27$churn_perc<-d27$n/d27$N
d27$varname<-rep("months",nrow(d27))
hist(d27$n)
ggplot(d27,aes(x=d27$dec,y=d27$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#totcalls
te%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->d28
d28$N<-unclass(te%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
d28$churn_perc<-d28$n/d28$N
d28$varname<-rep("totcalls",nrow(d28))
hist(d28$n)
ggplot(d28,aes(x=d28$dec,y=d28$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#custcare_Mean less than dec but will check in level of significate as customer care say that service quality 
te%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d29
hist(d29$n)

#callwait_Mean  less than dec
te%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d30
hist(d30$n)

#iwylis_vce_Mean less than dec
te%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d31
hist(d31$n)

#callwait_Range less than dec
te%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d32
hist(d32$n)

#ccrndmou_Range less than dec
te%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d33
hist(d33$n)

#adjqty
te%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->d34
d34$N<-unclass(te%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
d34$churn_perc<-d34$n/d34$N
d34$varname<-rep("adjqty",nrow(d34))
hist(d34$n)
ggplot(d34,aes(x=d34$dec,y=d34$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#avg3mou
te%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->d35
d35$N<-unclass(te%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
d35$churn_perc<-d35$n/d35$N
d35$varname<-rep("avg3mou",nrow(d35))
hist(d35$n)
ggplot(d35,aes(x=d35$dec,y=d35$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#avgmou
te%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->d36
d36$N<-unclass(te%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
d36$churn_perc<-d36$n/d36$N
d36$varname<-rep("avgmou",nrow(d36))
hist(d36$n)
ggplot(d36,aes(x=d36$dec,y=d36$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#avg3qty
te%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->d37
d37$N<-unclass(te%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
d37$churn_perc<-d37$n/d37$N
d37$varname<-rep("avg3qty",nrow(d37))
hist(d37$n)
ggplot(d37,aes(x=d37$dec,y=d37$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#avgqty
te%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->d38
d38$N<-unclass(te%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
d38$churn_perc<-d38$n/d38$N
d38$varname<-rep("avgqty",nrow(d38))
hist(d38$n)
ggplot(d38,aes(x=d38$dec,y=d38$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#actvsubs less than 10 need check for categoriable variable and will be check in level of significant
te%>%mutate(dec=ntile(actvsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)->d39
hist(d39$n)

#uniqsubs less than 10 need check for categoriable variable and will be check in level of significant
te%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)->d40
hist(d40$n)

#opk_dat_Mean less than 10
te%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d41
hist(d41$n)


#recv_sms_Mean less than 10
te%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d42
hist(d42$n)

#blck_dat_Mean less than 10
te%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d43
hist(d43$n)


#mou_pead_Mean less than 10
te%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d44
hist(d44$n)

#drop_dat_Mean less than 10
te%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d45
hist(d45$n)

#drop_vce_Mean
te%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d46
d46$N<-unclass(te%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
d46$churn_perc<-d46$n/d46$N
d46$varname<-rep("drop_vce_Mean",nrow(d46))
hist(d46$n)
ggplot(d46,aes(x=d46$dec,y=d46$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#adjmou
te%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->d47
d47$N<-unclass(te%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
d47$churn_perc<-d47$n/d47$N
d47$varname<-rep("adjmou",nrow(d47))
hist(d47$n)
ggplot(d47,aes(x=d47$dec,y=d47$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#totrev
te%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->d48
d48$N<-unclass(te%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
d48$churn_perc<-d48$n/d48$N
d48$varname<-rep("totrev",nrow(d48))
hist(d48$n)
ggplot(d48,aes(x=d48$dec,y=d48$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#totmrc_Mean
te%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d49
d49$N<-unclass(te%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]] 
d49$churn_perc<-d49$n/d49$N
d49$varname<-rep("totmrc_Mean",nrow(d49))
hist(d49$n)
ggplot(d49,aes(x=d49$dec,y=d49$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#rev_Range
te%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->d50
d50$N<-unclass(te%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]] 
d50$churn_perc<-d50$n/d50$N
d50$varname<-rep("rev_Range",nrow(d50))
hist(d50$n)
ggplot(d50,aes(x=d50$dec,y=d50$churn_perc))+geom_point(color='red')+geom_smooth()## x=dec,y=d1$churn_perc

#div_type need be reomved 
te%>%mutate(dec=ntile(div_type,n=10))%>%count(churn,dec)%>%filter(churn==1)->d51
hist(d51$n)

#ovrmou_Mean less that 10
te%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->d22
hist(d22$n)



## Variable Profiling: Categorical Variables( after find the unquie variable ued unquie formula)
#actvsubs
te%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC1
datC1$N<-unclass(te%>%filter(actvsubs%in%datC1$levels)%>%count(actvsubs))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("actvsubs",nrow(datC1))
te$asl_flag
#crclscod need omit 
te%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC2
datC2$Var.Name<-rep("crclscod",nrow(datC2))

#asl_flag  we need check the level of significate 
te%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC3
datC3$Var.Name<-rep("asl_flag",nrow(datC3))

#csa  we need to remove the as more level
te%>%count(churn,levels=csa)%>%filter(churn==1)->datC4
datC4$Var.Name<-rep("csa",nrow(datC4))

#refurb_new   we need check the level of significate 
te%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC5
datC5$N<-unclass(te%>%filter(refurb_new%in%datC1$levels)%>%count(refurb_new))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("refurb_new",nrow(datC5))
#marital  
te%>%count(churn,levels=marital)%>%filter(churn==1)->datC7
datC7$N<-unclass(te%>%filter(marital%in%datC1$levels)%>%count(marital))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("marital",nrow(datC7))
te
#ethnic  
te%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC8
datC8$N<-unclass(te%>%filter(ethnic%in%datC1$levels)%>%count(ethnic))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("ethnic",nrow(datC8))

#car_buy  
te%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC9
datC9$N<-unclass(te%>%filter(car_buy%in%datC1$levels)%>%count(car_buy))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("car_buy",nrow(datC9))

#hnd_price  
te%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC10
datC10$N<-unclass(te%>%filter(hnd_price%in%datC1$levels)%>%count(hnd_price))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("hnd_price",nrow(datC10))

#mtrcycle  
te%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC11
datC11$N<-unclass(te%>%filter(mtrcycle%in%datC1$levels)%>%count(mtrcycle))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("hnd_price",nrow(datC11))

#uniqsubs
te%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC12
datC12$N<-unclass(te%>%filter(uniqsubs%in%datC12$levels)%>%count(uniqsubs))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("uniqsubs",nrow(datC12))
#forgntvl
te%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC13
datC13$N<-unclass(te%>%filter(forgntvl%in%datC13$levels)%>%count(forgntvl))[[2]]
datC13$ChurnPerc<-datC1$n/datC1$N
datC13$Var.Name<-rep("forgntvl",nrow(datC13))
#retdays
te%>%count(churn,levels=retdays)%>%filter(churn==1)->datC15
datC15$N<-unclass(te%>%filter(retdays%in%datC15$levels)%>%count(retday1))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("retdays",nrow(datC15))
#models
te%>%count(churn,levels=models)%>%filter(churn==1)->datC16
datC16$N<-unclass(te%>%filter(models%in%datC16$levels)%>%count(models))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("models",nrow(datC16))
#income                    
te%>%count(churn,levels=income)%>%filter(churn==1)->datC17
datC17$N<-unclass(te%>%filter(income%in%datC17$levels)%>%count(income))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("income",nrow(datC17))
#truck
te%>%count(churn,levels=truck)%>%filter(churn==1)->datC18
datC18$N<-unclass(te%>%filter(truck%in%datC18$levels)%>%count(truck))[[2]]
datC18$ChurnPerc<-datC18$n/datC18$N
datC18$Var.Name<-rep("truck",nrow(datC18))
#prizm_social_one
te%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC19
datC19$N<-unclass(te%>%filter(prizm_social_one%in%datC19$levels)%>%count(prizm_social_one))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("prizm_social_one",nrow(datC19))
#area
te%>%count(churn,levels=area)%>%filter(churn==1)->datC20
datC20$N<-unclass(te%>%filter(area%in%datC20$levels)%>%count(area))[[2]]
datC20$ChurnPerc<-datC20$n/datC20$N
datC20$Var.Name<-rep("area",nrow(datC20))

## Missing Value treatment 

## treatement of outlier 

capoutlier= function(x){
  qnt=quantile(x,probs=c(.25,.75),na.rm = T)
  caps=quantile(x,probs = c(0.05,.95),na.rm = T)
  H=1.5 * IQR(x,na.rm = T)
  x[x<(qnt[1]-H)]=caps[1]
  x[x>(qnt[2]+H)]=caps[2]
  return(x)
}
capoutlier1= function(x){
  qnt=quantile(x,probs=c(.25,.75),na.rm = T)
  caps=quantile(x,probs = c(0.1,.90),na.rm = T)
  H=1.5 * IQR(x,na.rm = T)
  x[x<(qnt[1]-H)]=caps[1]
  x[x>(qnt[2]+H)]=caps[2]
  return(x)
}

capoutlier2= function(x){
  qnt=quantile(x,probs=c(.25,.75),na.rm = T)
  caps=quantile(x,probs = c(0.2,.80),na.rm = T)
  H=1.5 * IQR(x,na.rm = T)
  x[x<(qnt[1]-H)]=caps[1]
  x[x>(qnt[2]+H)]=caps[2]
  return(x)
}

## outlier treat with check of box plot
te$mou_Mean=capoutlier2(te$mou_Mean)
boxplot(te$mou_Mean)
te$change_mou=capoutlier1(te$change_mou)
boxplot(te$change_mou)
te$hnd_price=capoutlier1(te$hnd_price)
boxplot(te$hnd_price)
te$rev_Mean=capoutlier1(te$rev_Mean)
boxplot(te$rev_Mean)
te$totmrc_Mean=capoutlier2(te$totmrc_Mean)
boxplot(te$totmrc_Mean)
te$rev_Range=capoutlier1(te$rev_Range)
boxplot(te$rev_Range)
te$mou_Range=capoutlier1(te$mou_Range)
boxplot(te$mou_Range)
te$ovrrev_Mean=capoutlier1(te$ovrrev_Mean)
boxplot(te$ovrrev_Mean)
te$roam_Mean=capoutlier2(te$roam_Mean)
boxplot(te$roam_Mean)
te$mou_opkv_Range=capoutlier1(te$mou_opkv_Range)
boxplot(te$mou_opkv_Range)
te$adjrev=capoutlier1(te$adjrev)
boxplot(te$adjrev)
te$drop_blk_Mean=capoutlier1(te$drop_blk_Mean)
boxplot(te$drop_blk_Mean)
te$custcare_Mean=capoutlier2(te$custcare_Mean)
boxplot(te$custcare_Mean)
te$opk_dat_Mean=capoutlier1(te$opk_dat_Mean)
boxplot(te$opk_dat_Mean)
te$opk_dat_Mean=capoutlier1(te$opk_dat_Mean)
boxplot(te$opk_dat_Mean)
te$callwait_Mean=capoutlier2(te$callwait_Mean)
boxplot(te$callwait_Mean)
te$iwylis_vce_Mean=capoutlier1(te$iwylis_vce_Mean)
boxplot(te$iwylis_vce_Mean)
te$totrev=capoutlier1(te$totrev)
boxplot(te$totrev)

te$numbcars=capoutlier1(te$numbcars)
boxplot(te$numbcars)
# Based on the data discription merging cell COMPLETE_MEAN and ATTEMPT_MEAN
te$complete_mean=te$comp_dat_Mean+te$comp_vce_Mean
te$attempt_mean=te$plcd_dat_Mean+te$plcd_vce_Mean
##since we have merged the 4 column so we are removing same below
names(te)
te1=te[,-c(23,24,80,81)]

names(te1)
#Missing values for this variable can be assumed to mean there have been no retention calls made by the customer.
summary(te1$retdays)
te1$retdays=ifelse(is.na(te1$retdays)==T,0,1)
table(te1$retdays)
summary(te1$retdays)

## DROP_BLK_MEAN=BLCK_DAT_MEAN + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN
## Remove the drop_dat_Mean+drop_vce_Mean+blck_dat_Mean
## OVRREV_MEAN=DATOVR_MEAN + VCEOVR_MEAN
## remove DATOVR_MEAN + VCEOVR_MEAN
te1=te1[,-c(55,71,72,68)]
## call mints per mint Avg3qty/Avg3mou
te1$Avg3min= round(te1$avg3mou/te1$avg3qty)
te1$Avg3min[te1$Avg3min=="Inf"]= 0
te1$Avg3min[is.na(te1$Avg3min)]= 0


## removeing the information where NA is 30% missing
#deleting missig variable  COl are Div type, solfag, wrk women,proptype, occu1

rm(miss)
miss <- c()
for (i in 1:ncol(te1)) {
  if(length(which(is.na(te1[,i]))) > 0.70*nrow(te1)) miss = append(miss,i)
  
}
te2=te1[,-miss]
table(is.na(te1$div_type))
table(is.na(te1$solflag))
table(is.na(te1$wrkwoman))
table(is.na(te1$proptype))
table(is.na(te1$occu1))

colSums(is.na(te2))



# Missing value imputation by median and mode
var_num= select_if(te2,is.numeric)
var_num1=names(var_num)

var_char= select_if(te2,is.character)
var_char1=names(var_char)

for (k in names(te2)) {
  if(k %in% var_num1){
    #impute numeric variable with median
    med=median(te2[[k]],na.rm=T)
    set(x=te2,which(is.na(te2[[k]])),k,med)
  }else if(k %in% var_char1)
    # imput categorical Variable with mode
    mode=names(which.max(table(te2[[k]])))
  set(x=te2,which(is.na(te2[[k]])),k,mode)
}


## age1 is conveting into Default young Mid Age and old

te2$age1_1=ifelse(te2$age1==0,"Default",ifelse(te2$age1<=25,"young",ifelse(te2$age1>25&te2$age1<=50,"MidAge","Old")))
te$age1_1=as.factor(te2$age1_1)


## check for the unique varible and converting factors them
te2$models=as.factor(te2$models)
unique(te2$hnd_price)
class(te2$hnd_price)
te2$hnd_price=as.factor(te2$hnd_price)
unique(te2$forgntvl)
te2$forgntvl=as.factor(te2$forgntvl)
unique(te2$mtrcycle)
te2$mtrcycle=as.factor(te2$mtrcycle)
unique(te2$truck)
te2$truck=as.factor(te2$truck)
unique(te2$retdays)
te2$retdays=as.factor(te2$retdays)
unique(te2$prizm_social_one)
te2$prizm_social_one=as.factor(te2$prizm_social_one)
unique(te2$area)
te2$area=as.factor(te2$area)
unique(te2$asl_flag)
te2$asl_flag=as.factor(te2$asl_flag)
unique(te2$refurb_new)
te2$refurb_new=as.factor(te2$refurb_new)
unique(te2$hnd_webcap)
te2$hnd_webcap=as.factor(te2$hnd_webcap)
unique(te$marital)
te2$marital=as.factor(te2$marital)
unique(te2$models)
te2$models=as.factor(te2$models)
unique(te$ethnic)
te2$ethnic=as.factor(te2$ethnic)
unique(te2$car_buy)
te2$car_buy=as.factor(te2$car_buy)
te2$actvsubs=as.factor(te2$actvsubs)
names(te2)
##deleting the data from data from non significatin mailresp,mailordr as model will not run
te2=te2[,-c(46,56)]
## since i am getting error and also crclsod has multicornitry hence removing 
te2=te2[,-(29)]
te2=te2[,-(57)]
str(te2)
set.seed(300)
index=sample(nrow(te2),0.70*nrow(te2),replace=F)
train=te2[index,]
test=te2[-index,]
nrow(train)
nrow(test)
table(te2$churn)
table(train$churn)/46407
table(test$churn)/19890

colSums(is.na(train)) 



#running frist model
mod=glm(train$churn~.,data = train,family = "binomial")
summary(mod)
modv1=glm(train$churn~ Avg3min+hnd_price+complete_mean+age1_1+avgrev+children+mou_pead_Mean
          +retdays+uniqsubs+actvsubs+ethnic+refurb_new+area+prizm_social_one+iwylis_vce_Mean
          +avg6qty+avgmou+avg3mou+ovrrev_Mean+custcare_Mean+eqpdays+months+mou_opkv_Range
          +owylis_vce_Range+drop_blk_Mean+change_mou+mou_Range+rev_Range+totmrc_Mean+mou_Mean
          +adjqty+rev_Mean+asl_flag,
          data = train,family = "binomial")
summary(modv1)
vif(modv1)
##,avg3mou,avgmou,avg6qty,
## insignificate adjqty,rev_Mean
mod1=glm(train$churn~ models+dwllsize+hnd_price+complete_mean+age1_1+avgrev+
           children+retdays+uniqsubs+actvsubs+ethnic+refurb_new+area+prizm_social_one+
           iwylis_vce_Mean+avgmou+ovrrev_Mean+custcare_Mean+eqpdays+months+asl_flag+
           mou_opkv_Range+owylis_vce_Range+drop_blk_Mean+change_mou+mou_Range+rev_Range+Avg3min+
           totmrc_Mean+mou_Mean,data = train,family = "binomial")
summary(mod1)
vif(mod1)





##mou_Mean+
## Since there more level in hnd_price wont all my model to run perd so creating dummy
## creating dummy for sinificate value

train$prizm_social_oneR= ifelse(train$prizm_social_one=="R",1,0)
test$prizm_social_oneR= ifelse(test$prizm_social_one=="R",1,0)

train$prizm_social_oneT= ifelse(train$prizm_social_one=="T",1,0)
test$prizm_social_oneT= ifelse(test$prizm_social_one=="T",1,0)

unique(train$models)
train$models2= ifelse(train$models=="2",1,0)
test$models2= ifelse(test$models=="2",1,0)
train$models3= ifelse(train$models=="3",1,0)
test$models3= ifelse(test$models=="3",1,0)
train$models4= ifelse(train$models=="4",1,0)
test$models4= ifelse(test$models=="4",1,0)
train$models5= ifelse(train$models=="5",1,0)
test$models5= ifelse(test$models=="5",1,0)

unique(train$ethnic)
train$ethnicC= ifelse(train$ethnic =="C",1,0)
test$ethnicC = ifelse(test$ethnic =="C",1,0)
train$ethnicJ= ifelse(train$ethnic =="J",1,0)
test$ethnicJ = ifelse(test$ethnic =="J",1,0)
train$ethnicG= ifelse(train$ethnic =="G",1,0)
test$ethnicG = ifelse(test$ethnic =="G",1,0)
train$ethnicH= ifelse(train$ethnic =="H",1,0)
test$ethnicH = ifelse(test$ethnic =="H",1,0)
train$ethnicI= ifelse(train$ethnic =="I",1,0)
test$ethnicI = ifelse(test$ethnic =="I",1,0)
train$ethnicN= ifelse(train$ethnic =="N",1,0)
test$ethnicN = ifelse(test$ethnic =="N",1,0)
train$ethnicS= ifelse(train$ethnic =="S",1,0)
test$ethnicS = ifelse(test$ethnic =="S",1,0)
train$ethnicZ= ifelse(train$ethnic =="Z",1,0)
test$ethnicZ = ifelse(test$ethnic =="Z",1,0)
train$ethnicU= ifelse(train$ethnic =="U",1,0)
test$ethnicU = ifelse(test$ethnic =="U",1,0)

unique(train$hnd_price)
train$hnd_price59.98999023=ifelse(train$hnd_price=="59.98999023",1,0)
test$hnd_price59.98999023=ifelse(test$hnd_price=="59.98999023",1,0)

train$hnd_price79.98999023=ifelse(train$hnd_price=="79.98999023",1,0)
test$hnd_price79.98999023=ifelse(test$hnd_price=="79.98999023",1,0)

train$hnd_price99.98999023=ifelse(train$hnd_price=="99.98999023",1,0)
test$hnd_price99.98999023=ifelse(test$hnd_price=="99.98999023",1,0)

train$hnd_price129.9899902=ifelse(train$hnd_price=="129.9899902",1,0)
test$hnd_price129.9899902=ifelse(test$hnd_price=="129.9899902",1,0)
train$hnd_price149.9899902=ifelse(train$hnd_price=="149.9899902",1,0)
test$hnd_price149.9899902=ifelse(test$hnd_price=="149.9899902",1,0)
train$hnd_price199.9899902=ifelse(train$hnd_price=="199.9899902",1,0)
test$hnd_price199.9899902=ifelse(test$hnd_price=="199.9899902",1,0)
train$hnd_price249.9899902=ifelse(train$hnd_price=="249.9899902",1,0)
test$hnd_price249.9899902=ifelse(test$hnd_price=="249.9899902",1,0)
class(train$uniqsubs)
unique(train$uniqsubs)
train$uniqsubs2=ifelse(train$uniqsubs=="2",1,0)
test$uniqsubs2=ifelse(test$uniqsubs=="2",1,0)
train$uniqsubs3=ifelse(train$uniqsubs=="3",1,0)
test$uniqsubs3=ifelse(test$uniqsubs=="3",1,0)
train$uniqsubs4=ifelse(train$uniqsubs=="4",1,0)
test$uniqsubs4=ifelse(test$uniqsubs=="4",1,0)
train$uniqsubs5=ifelse(train$uniqsubs=="5",1,0)
test$uniqsubs5=ifelse(test$uniqsubs=="5",1,0)
train$uniqsubs7=ifelse(train$uniqsubs=="7",1,0)
test$uniqsubs7=ifelse(test$uniqsubs=="7",1,0)
train$uniqsubs9=ifelse(train$uniqsubs=="9",1,0)
test$uniqsubs9=ifelse(test$uniqsubs=="9",1,0)



class(train$actvsubs)
unique(train$actvsubs)
train$actvsubs1=ifelse(train$actvsubs=="1",1,0)
test$actvsubs1=ifelse(test$actvsubs=="1",1,0)
train$actvsubs2=ifelse(train$actvsubs=="2",1,0)
test$actvsubs2=ifelse(test$actvsubs=="2",1,0)
train$actvsubs3=ifelse(train$actvsubs=="3",1,0)
test$actvsubs3=ifelse(test$actvsubs=="3",1,0)
train$actvsubs4=ifelse(train$actvsubs=="4",1,0)
test$actvsubs4=ifelse(test$actvsubs=="4",1,0)
train$actvsubs5=ifelse(train$actvsubs=="5",1,0)
test$actvsubs5=ifelse(test$actvsubs=="5",1,0)
train$actvsubs6=ifelse(train$actvsubs=="6",1,0)
test$actvsubs6=ifelse(test$actvsubs=="6",1,0)
train$actvsubs7=ifelse(train$actvsubs=="7",1,0)
test$actvsubs7=ifelse(test$actvsubs=="7",1,0)
train$actvsubs9=ifelse(train$actvsubs=="9",1,0)
test$actvsubs9=ifelse(test$actvsubs=="9",1,0)
train$actvsubs10=ifelse(train$actvsubs=="10",1,0)
test$actvsubs10=ifelse(test$actvsubs=="10",1,0)
train$actvsubs8=ifelse(train$actvsubs=="8",1,0)
test$actvsubs8=ifelse(test$actvsubs=="8",1,0)

class(train$dwllsizeI)
train$dwllsizeI=ifelse(train$dwllsize=="I",1,0)
test$dwllsizeI=ifelse(test$dwllsize=="I",1,0)

train$areaSFA=ifelse(train$area=="SOUTH FLORIDA AREA",1,0)
test$areaSFA=ifelse(test$area=="SOUTH FLORIDA AREA",1,0)
train$areaNW=ifelse(train$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$areaNW=ifelse(test$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
train$areaCNW=ifelse(train$area=="CALIFORNIA NORTH AREA",1,0)
test$areaCNW=ifelse(test$area=="CALIFORNIA NORTH AREA",1,0)
train$areaNYA=ifelse(train$area=="NEW YORK CITY AREA",1,0)
test$areaNYA=ifelse(test$area=="NEW YORK CITY AREA",1,0)
train$areaTX=ifelse(train$area=="CENTRAL/SOUTH TEXAS AREA",1,0)
test$areaTX=ifelse(test$area=="CENTRAL/SOUTH TEXAS AREA",1,0)
train$areaTEA=ifelse(train$area=="TENNESSEE AREA",1,0)
test$areaTEA=ifelse(test$area=="TENNESSEE AREA",1,0)

unique(train$age1_1)
train$age1_1M=ifelse(train$age1_1=="MidAge",1,0)
test$age1_1M=ifelse(test$age1_1=="MidAge",1,0)
train$age1_1Y=ifelse(train$age1_1=="young",1,0)
test$age1_1Y=ifelse(test$age1_1=="young",1,0)
train$age1_1O=ifelse(train$age1_1=="Old",1,0)
test$age1_1O=ifelse(test$age1_1=="Old",1,0)

unique(train$asl_flag)
train$asl_flag_Y=ifelse(train$asl_flag== "Y",1,0)
test$asl_flag_Y=ifelse(test$asl_flag== "Y",1,0)
unique(train$refurb_new)
train$refurb_new_R=ifelse(train$refurb_new=="R",1,0)
test$refurb_new_R=ifelse(test$refurb_new=="R",1,0)

mod2=glm(train$churn~ hnd_price59.98999023+hnd_price59.98999023+hnd_price249.9899902+area+asl_flag
         +hnd_price199.9899902+hnd_price149.9899902+hnd_price129.9899902+hnd_price99.98999023+
           hnd_price79.98999023+complete_mean+age1_1+Avg3min+children+retdays+uniqsubs+actvsubs+
           ethnic+refurb_new+area+prizm_social_one+iwylis_vce_Mean+ovrrev_Mean+custcare_Mean+avgmou+
           eqpdays+months+mou_opkv_Range+owylis_vce_Range+drop_blk_Mean+change_mou+mou_Range+models+dwllsize+
           +totmrc_Mean+mou_Mean,data = train,family = "binomial")
summary(mod2)
vif(mod2)

mod3=glm(train$churn~ models2+models3+models4+hnd_price59.98999023+hnd_price59.98999023+hnd_price249.9899902+asl_flag
         +hnd_price199.9899902+hnd_price149.9899902+hnd_price129.9899902+hnd_price99.98999023+
           hnd_price79.98999023+complete_mean+Avg3min+children+retdays+uniqsubs+actvsubs+
           ethnicC+ethnicJ+ethnicG+ethnicH+ethnicI+prizm_social_oneR+prizm_social_oneT+
           +areaSFA+areaNW+areaNYA+areaTEA+
         +ethnicN+ethnicS+ethnicZ+ethnicU+age1_1M+age1_1O+
           refurb_new+iwylis_vce_Mean+avgmou+ovrrev_Mean+custcare_Mean+
           eqpdays+months+mou_opkv_Range+owylis_vce_Range+drop_blk_Mean+change_mou+mou_Range+dwllsizeI+
           +totmrc_Mean+mou_Mean,data = train,family = "binomial")
summary(mod3)
vif(mod3)

train$uniqsubs=as.factor(train$uniqsubs)
train$actvsubs=as.numeric(train$actvsubs)

mod4=glm(train$churn~ models2+models3+models4+hnd_price59.98999023+hnd_price59.98999023+hnd_price249.9899902+asl_flag
         +hnd_price199.9899902+hnd_price149.9899902+hnd_price129.9899902+hnd_price99.98999023+
           hnd_price79.98999023+complete_mean+Avg3min+children+retdays+
           ethnicC+ethnicJ+ethnicG+ethnicH+ethnicI+prizm_social_oneR+prizm_social_oneT+
           +areaSFA+areaNW+areaNYA+areaTEA++uniqsubs2++uniqsubs3+uniqsubs4+uniqsubs5+uniqsubs7+uniqsubs9
           +ethnicN+ethnicS+ethnicZ+ethnicU+age1_1M+age1_1O+avgmou+
           refurb_new+iwylis_vce_Mean+avgmou+ovrrev_Mean+custcare_Mean+
           actvsubs1+actvsubs2+actvsubs3+actvsubs4+actvsubs5+actvsubs6+actvsubs7+actvsubs8+actvsubs9+actvsubs10+
           eqpdays+months+mou_opkv_Range+owylis_vce_Range+drop_blk_Mean+change_mou+mou_Range+dwllsizeI+
           +totmrc_Mean+mou_Mean,data = train,family = "binomial")
summary(mod4)
vif(mod4)
###After actvsub converted to the numerice became insignicate in all levels
mod5=glm(train$churn~ models2+models3+models4+hnd_price59.98999023+hnd_price59.98999023+hnd_price249.9899902+asl_flag
         +hnd_price199.9899902+hnd_price149.9899902+hnd_price129.9899902+hnd_price99.98999023+
           hnd_price79.98999023+complete_mean+Avg3min+children+retdays+
           ethnicC+ethnicJ+ethnicG+ethnicH+ethnicI+prizm_social_oneR+prizm_social_oneT+
           +areaSFA+areaNW+areaNYA+areaTEA++uniqsubs2++uniqsubs3+uniqsubs4+uniqsubs5+uniqsubs7+uniqsubs9
         +ethnicN+ethnicS+ethnicZ+ethnicU+age1_1M+age1_1O+avgmou+
           refurb_new+iwylis_vce_Mean+avgmou+ovrrev_Mean+custcare_Mean+
           eqpdays+months+mou_opkv_Range+owylis_vce_Range+drop_blk_Mean+change_mou+mou_Range+dwllsizeI+
           +totmrc_Mean+mou_Mean,data = train,family = "binomial")
summary(mod5)
vif(mod5)

mod6=glm(train$churn~ models2+models3+models4+hnd_price59.98999023+hnd_price59.98999023+hnd_price249.9899902+asl_flag
          +hnd_price199.9899902+hnd_price149.9899902+hnd_price129.9899902+hnd_price99.98999023+
            hnd_price79.98999023+complete_mean+Avg3min+children+retdays+
            ethnicC+ethnicJ+ethnicG+ethnicH+ethnicI+prizm_social_oneR+prizm_social_oneT+
            +areaSFA+areaNW+areaNYA+areaTEA++uniqsubs2++uniqsubs3+uniqsubs4+uniqsubs5+
          +ethnicN+ethnicS+ethnicZ+ethnicU+age1_1M+age1_1O+avgmou+
            refurb_new+iwylis_vce_Mean+avgmou+ovrrev_Mean+custcare_Mean+
            eqpdays+months+mou_opkv_Range+owylis_vce_Range+drop_blk_Mean+change_mou+mou_Range+dwllsizeI+
            +totmrc_Mean+mou_Mean,data = train,family = "binomial")
summary(mod6)
vif(mod6)

mod7=glm(train$churn~ models2+models3+models4+hnd_price59.98999023+hnd_price59.98999023
         +hnd_price249.9899902+asl_flag+hnd_price199.9899902+hnd_price149.9899902
         +hnd_price129.9899902+hnd_price99.98999023+hnd_price79.98999023
         +complete_mean+Avg3min+children+retdays+ethnicC+ethnicJ+ethnicG+ethnicH+ethnicI+prizm_social_oneR+prizm_social_oneT+
            +areaSFA+areaNW+areaNYA+areaTEA++uniqsubs2++uniqsubs3+uniqsubs4+uniqsubs5+
            +ethnicN+ethnicS+ethnicZ+ethnicU+age1_1M+age1_1O+
            refurb_new+iwylis_vce_Mean+avgmou+ovrrev_Mean+custcare_Mean+
            eqpdays+months+mou_opkv_Range+owylis_vce_Range+drop_blk_Mean
            +change_mou+mou_Range+dwllsizeI+avgmou+
            +totmrc_Mean+mou_Mean,data = train,family = "binomial")
summary(mod7)
vif(mod7)
##to Check confident of the model 
#confint(mod7)

## prediction
library(ROCR)
library(caret)

pred=predict(mod7,type="response",newdata=test)
hist(pred)

pred1=ifelse(pred>=0.215050910623,1,0)
pred2=as.factor(pred1)


## After several itteration in cut-off values, the model is perdicting cut off

##Kappa matrix
library(irr)

kappa2(data.frame(test$churn,pred1))

test$churn=as.factor(test$churn)
confusionMatrix(pred2,test$churn,positive = "1")
pred=prediction(pred,test$churn)
eval=performance(pred,"acc")
plot(eval)
abline(h=0.76,v=0.54)

##Roc Curve
roc=performance(pred,"tpr","fpr")
plot(roc,colorize=T)
abline(a=0,b=1)

##Area under Curve
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))
auc

# AUC is 62.88% which is more than 50% 
# Model is seem like be perfoming ok and can be acceptable

#*______________________________________________________________________________*#

#1)What are the top Five factors driving likelihood of churn at Mobicom?
##A generic method for calculating variable importance for objects produced by train and method specific methods
varImp(mod7)
# 2
mod7$coefficients

#4
# Testing the Probability  

test$prob<-predict(mod7,type="response",newdata=test)

quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#applying cutoff value  to predict customers who will

pred4=predict(mod7,type = "response", newdata = test)
pred4=ifelse(pred4>=0.2314204,1,0)

## we can top frequency data is there between 0.2 to 0.3


Targeted=test[test$prob>0.2314204&test$prob<=0.7735759 & test$churn=="1","Customer_ID"]
Targeted= as.data.frame(Targeted)
write.csv(Targeted,"Target_customers1.csv",row.names=F)


#Using the model can be used to predict cusomers with high probability churn and extract the
#target list using their "Customer ID"


# 5
pred5=predict(mod7,type = "response",newdata = test)
test$prob=predict(mod7,type = "response",newdata = test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6=ifelse(pred5<0.20,"low_score",ifelse(pred5>=0.20 & pred5<0.30,
                                             "Medium_Score","High_score"))
table(pred6,test$churn)

str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Rev_levels=ifelse(test$totrev<560.592,"low_Rev",ifelse(test$totrev>560.592 &
                                                               test$totrev<1138.156, "Medium_Rev","High_Rev"))
table(Rev_levels)
table(pred6,Rev_levels)

## This table can be used to select the levels of customers are to be trageted

test$prob_levels=ifelse(pred5<0.20,"low_score",ifelse(pred5>=0.20 & pred5<0.30, "Medium_score","High_score"))
test$Rev_levels=ifelse(test$totrev<560.592,"low_Rev",ifelse(test$totrev>560.592 &
                                                              test$totrev<1138.156, "Medium_Rev","High_Rev"))

Targeted_1=test[test$prob_levels=="High_score"&test$Rev_levels=="High_Rev","Customer_ID"]

Targeted_1=as.data.frame(Targeted_1)
nrow(Targeted_1)

write.csv(Targeted_1,"High_Rev_Target_Customers.csv",row.names=F)



