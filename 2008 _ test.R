library(dplyr)
#library(readr)
data_08<-read.csv("D:\\AData\\2008.csv")
weather_08<-read.csv("D:\\AData\\DFW_2008.csv")

colnames(data_08)<-tolower(colnames(data_08))
colnames(weather_08)<-tolower(colnames(weather_08))

mydata<-data_08
data_08<-mydata

table(data_08$weatherdelay,data_08$origin)  # getting proportion of weatherdelay  0 and 1
data_08<-filter(data_08,origin=="DFW")

grep("weatherdelay",colnames(data_08))
boxplot(data_08$weatherdelay,outline=TRUE)

data_08 <- data_08[data_08$weatherdelay<250,]
boxplot(data_08$weatherdelay,outline=TRUE)

table(data_08$weatherdelay) 

data_No_0_8<-filter(data_08, depdelay>0)
nrow(data_No_0_8)
#table(data_No_0_8$weatherdelay) 

#data_No_0_8<-filter(data_No_0_8,deptime !="NA" & depdelay!="NA")#...no effect

data_No_0_8$dates<-paste(data_No_0_8$year ,data_No_0_8$month,data_No_0_8$dayofmonth,sep="-")
data_No_0_8$dates<-as.Date(data_No_0_8$dates)

#------data_No_0_8 BACKUP.......

#mydata_No_0_8<-data_No_0_8
mydata <- data_No_0_8

data_No_0_8<-mydata
#data_No_0_8 <- read.csv("D:\\15-mar1.csv") # be careful

#....BUCKETING.........
table(data_No_0_8$weatherdelay)

grep("weatherdelay",colnames(data_No_0_8))
summary(data_No_0_8$weatherdelay)
high=10
low=1
df_1=data.frame()
df_1 <- filter(mydata,weatherdelay== 0)
table(mydata$weatherdelay)
while(high<241)
{
  df=mydata[mydata$weatherdelay>=low & mydata$weatherdelay<high,]
  #df = filter(mydata,weatherdelay>=low & weatherdelay<high)
  c <-nrow(df)
  df[,26]=mean(df$weatherdelay)
  df_1<-rbind(df,df_1)
  high=high+10
  low=low+10
}
nrow(df_1)
unique(df_1$weatherdelay)
df_1$weatherdelay <- round(df_1$weatherdelay)
unique(mydata$weatherdelay)


names(weather_08)
weather_08$cst<-as.Date(weather_08$cst)        #----CONVERTING TO DATE FORMAT
weather_08<- rename(weather_08, dates=cst)

names(data_No_0_8)
data<-select(df_1,c(30,26))
mydata<-inner_join(df_1,weather_08,by='dates')
names(df_1)
nrow(mydata)
airdata<-mydata #after inner Join

table(mydata$weatherdelay)
#write.csv(mydata,"D:\\29-mar1.csv")

write.csv(mydata,"D:\\Atest.csv")

names(mydata)


#boxplot(mydata$max.temperaturef)


colnames(mydata)
nrow(mydata)
grep("max.temperaturef",colnames(mydata))

#-----temp-----
high=41
low=31
maxi <- max(mydata$max.temperaturef)
df_1=data.frame()
while(high<maxi)
{
  df=mydata[mydata$max.temperaturef>=low & mydata$max.temperaturef<high,]
  df[,31]=median(df$max.temperaturef)
  df_1<-rbind(df,df_1)
  high=high+10
  low=low+10
}

high
unique(df_1$max.temperaturef)
unique(mydata$max.temperaturef)

#-----humidity--------
summary(df_1$max.humidity)
grep("max.humidity",colnames(df_1))
high=48
low=38
maxi <- max(mydata$max.humidity)

df_2=data.frame()
while(high<maxi)
{
  df2=df_1[df_1$max.humidity>=low & df_1$max.humidity<high,]
  df2[,37]=median(df2$max.humidity)
  df_2<-rbind(df2,df_2)
  
  high=high+10
  low=low+10
}
unique(df_2$max.humidity)
#----wind speed---
summary(df_2$mean.wind.speedmph)
grep("mean.wind.speedmph",colnames(df_2))

high=4
low=2
df_3=data.frame()
maxi <- max(mydata$mean.wind.speedmph)
while(high<maxi)
{
  df3=df_2[df_2$mean.wind.speedmph>=low & df_2$mean.wind.speedmph<high,]
  df3[,47]=median(df3$mean.wind.speedmph)
  df_3<-rbind(df3,df_3)
  
  high=high+2
  low=low+2
}
unique(df_3$mean.wind.speedmph)
#---bucket weather DElay and MAx Temp------------------------------------
write.csv(df_1,"D:\\21-3-16.csv")
write.csv(df_3,"D:\\Atest.csv")

#------------------------EVENT FOG----------------------------------------
names(df_1)
prob_1 <- filter(df_1,precipitationin == "0.00")
prob_1 <- filter(prob_1,max.temperaturef >= 65 & max.temperaturef <=87)
prob_1 <- filter(prob_1,max.humidity >= 90 & max.humidity <=100)
prob_1 <- filter(prob_1,mean.wind.speedmph >= 7 & mean.wind.speedmph <=9)
prob_1 <- filter(prob_1,min.sea.level.pressurein == 29.68 | min.sea.level.pressurein ==29.94 | min.sea.level.pressurein == 29.77 | min.sea.level.pressurein ==29.86)
#prob_1 <- filter(prob_1,events=="Fog")
nrow(prob_1)
table(prob_1$weatherdelay)
table(prob_1$weatherdelay,prob_1$events)
#----------------------------------------------------------------------

#------------------------EVENT FOG Rain----------------------------------------
names(df_1)
prob_3 <- filter(df_1,precipitationin == "0.30" )
prob_3 <- filter(prob_3,max.temperaturef == 35)
prob_3 <- filter(prob_3,max.humidity == 92)
prob_3 <- filter(prob_3,mean.wind.speedmph == 8)
prob_3 <- filter(prob_3,min.sea.level.pressurein == 29.78)

#prob_1 <- filter(prob_1,events=="Fog")
nrow(prob_3)
table(prob_3$weatherdelay)
table(prob_3$weatherdelay,prob_3$events)
#----------------------------------------------------------------------

#------------------------FOG Rain snow----------------------------------------
names(df_1)
prob_3 <- filter(df_1,precipitationin == "0.30" )
prob_3 <- filter(prob_3,max.temperaturef == 35)
prob_3 <- filter(prob_3,max.humidity == 92)
prob_3 <- filter(prob_3,mean.wind.speedmph == 8)
prob_3 <- filter(prob_3,min.sea.level.pressurein == 29.78)

#prob_1 <- filter(prob_1,events=="Fog")
nrow(prob_3)
table(prob_3$weatherdelay)
table(prob_3$weatherdelay,prob_3$events)
#----------------------------------------------------------------------

#------------------------FOG Rain Thunderstrom----------------------------------------
names(df_1)
df_1$precipitationin <- as.numeric(df_1$precipitationin)
prob_4 <- filter(df_1,precipitationin >= "0.35" & precipitationin <= "2.54")
prob_4 <- filter(prob_4,max.temperaturef >= 35 & max.temperaturef >= 87)
prob_4 <- filter(prob_4,max.humidity >=84 & max.humidity <=100)
prob_4 <- filter(prob_4,mean.wind.speedmph >= 6 & mean.wind.speedmph <= 16)
prob_4 <- filter(prob_4,min.sea.level.pressurein >= 29.68 & min.sea.level.pressurein <= 30.14)

#prob_1 <- filter(prob_1,events=="Fog")
nrow(prob_4)
table(prob_4$weatherdelay)
table(prob_4$weatherdelay,prob_4$events)
#----------------------------------------------------------------------
#-------------------MODEL---------------------------------------------
head(df_1$weatherdelay)
unique(df_1$weatherdelay)
df_Binary <- df_1
grep("weatherdelay",colnames(df_1))
df_Binary[df_Binary$weatherdelay>=6,2] <- 1
unique(df_Binary$weatherdelay)
#-------Weather delay is in 0 and 1-------------
table(df_Binary$weatherdelay)
imp_var <- c(2,3,9,19,21,23,14)
df_Binary <- df_Binary[,imp_var]
names(df_Binary)
#------------------------------------------------------------------------
indexes=sample(1:nrow(df_Binary),size=0.3*nrow(df_Binary))

test=df_Binary[indexes,]
train=df_Binary[-indexes,]
model_1 <- glm(train$weatherdelay~.,data =train)
summary(model_1)
pred_prob<-predict(model_1, newdata=test, type="response")
a <- round(pred_prob)
b <- test$weatherdelay
table(a==b)
head(a,20)
head(b,20)

test<-mydata
names(mydata)
