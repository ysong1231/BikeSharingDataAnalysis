################################################
###                                          ###
### Author: Yilin Song, syilin1231@gmail.com ###
### Date: 2019-03                            ###
###                                          ###
################################################

#########################################
#### Part 1: Descriptive Statistics #####
#########################################

#read data from trip
trip = read.csv("Data/trip.csv")

#descrip the user profiles by gender
library(plotrix)
library(dplyr)
by_gender<-group_by(subset(trip,trip$gender=="Female"|trip$gender=="Male"),gender)
gendercount<-summarise(by_gender,length(trip_id))
colnames(gendercount)<-c("gender","count")
Pct<-round(gendercount$count/sum(gendercount$count)*100,2)
Labs<-paste(c("Female","Male"),Pct,"%"," ")
pie3D(gendercount$count,labels = Labs,explode = 0.1,labelcex = 1)

#descrip the user profiles by membership type
by_user<-group_by(trip,usertype)
usercount<-summarise(by_user,length(trip_id))
colnames(usercount)<-c("usertype","count")
Pct1<-round(usercount$count/sum(usercount$count)*100,2)
Labs<-paste(c("Member","Short-term"),Pct1,"%"," ")
pie3D(usercount$count,labels = Labs,explode = 0.1)

#is several people ride together by gender
library(ggplot2)
library(plyr)
ggplot(data=trip,aes(x=usertype,fill=is_several_people))+geom_bar()
ggplot(data=trip[which(is.na(trip$gender)==FALSE),],aes(x=gender,fill=is_several_people))+geom_bar()

#is several people ride together by usertype
ggplot(data=trip,aes(x=usertype,fill=is_several_people))+geom_bar()
ggplot(data=trip[which(is.na(trip$usertype)==FALSE),],aes(x=usertype,fill=is_several_people))+geom_bar()

#daily ride volume pattern by date
library(plyr)
library(scales)
by_date<-ddply(trip,"starttime_date",summarise,N = length(trip_id))
qplot(as.Date(starttime_date),N,data=by_date,geom="line",group = 1) + xlab("Date") + ylab("Daily Trip Volume")+ scale_x_date(breaks = date_breaks("3 months"))


#####################################################
#### Part 2: Regression--Predict Rental Volume#####
#####################################################

#import data
station = read.csv("Data/station.csv")
start.data = read.csv("Data/predic_start.csv")
end.data = read.csv("Data/predic_end.csv")

########################################
###########Usage prediction for each station(linear regression) 

#summary of the data
summary(start.data)

#"Max_Gust_Speed_MPH" is factor with many missing values, change it into integer and delete missing value
start.data$Max_Gust_Speed_MPH = as.integer(start.data$Max_Gust_Speed_MPH)
start.data = start.data[!is.na(start.data$Max_Gust_Speed_MPH),]

#"starttime_wday" is factor, create dummy variables for it
summary(start.data$starttime_wday)
start.data$is_Mon = rep(0, 23820)
start.data$is_Mon[start.data$starttime_wday == "Mon"] = 1
start.data$is_Tues = rep(0, 23820)
start.data$is_Tues[start.data$starttime_wday == "Tues"] = 1
start.data$is_Wed = rep(0, 23820)
start.data$is_Wed[start.data$starttime_wday == "Wed"] = 1
start.data$is_Thur = rep(0, 23820)
start.data$is_Thur[start.data$starttime_wday == "Thur"] = 1
start.data$is_Fri = rep(0, 23820)
start.data$is_Fri[start.data$starttime_wday == "Fri"] = 1
start.data$is_Sat = rep(0, 23820)
start.data$is_Sat[start.data$starttime_wday == "Sat"] = 1

#"Events" is factor, create dummy variables for it
summary(start.data$Events)
start.data$Fog = rep(0, 23820)
start.data$Fog[start.data$Events == "Fog"] = 1
start.data$Fog_Rain = rep(0, 23820)
start.data$Fog_Rain[start.data$Events == "Fog-Rain"|start.data$Events == "Fog , Rain"] = 1
start.data$Rain = rep(0, 23820)
start.data$Rain[start.data$Events == "Rain"] = 1
start.data$Rain_Thunderstorm = rep(0, 23820)
start.data$Rain_Thunderstorm[start.data$Events == "Rain-Thunderstorm"] = 1
start.data$Snow_Rain = rep(0, 23820)
start.data$Snow_Rain[start.data$Events == "Rain , Snow Rain"] = 1
start.data$Thunderstorm = rep(0, 23820)
start.data$Thunderstorm[start.data$Events == "Thunderstorm"] = 1

#Since the month influences the usage, create month as another predictor
library(lubridate)
start.data$month = month(as.Date(start.data$Date))

#Select all integer coloums for linear regerssion and divide into training and testing
set.seed(1)
#just remove all the missing values
data.lm = na.omit(start.data[, c(3,6,9,12,15,18,21,23,25:37)])
train = sample(1:nrow(data.lm), 0.9*nrow(data.lm))
data.lm.train = data.lm[train, ]
data.lm.test = data.lm[-train,]

#try linear regression model with all predictors to predict rental volume
lm.all = lm(Count~., data = data.lm.train)
summary(lm.all)
#the R^2 is only around 0.17 and the mean sqrt erro is 5.6 which is not good
start.lm.pred = predict(lm.all, newdata = data.lm.test)
sqrt(mean((start.lm.pred - data.lm.test$Count)^2))

#not good for the full data, try to find the best model
library(leaps)
regfit = regsubsets(Count~., data = data.lm, nvmax = ncol(data.lm)-1)
regfit.summary = summary(regfit)
#find the max adjr2
which.max(regfit.summary$adjr2)
#plot the adjr2s and us red markers for the biggest one
plot(regfit.summary$adjr2, col = c(rep(1,10), 2), pch = 19, cex = 2)
#show the best model's parameters
coef(regfit, 11)

#try backward methord
regfit.back = regsubsets(Count~., data = data.lm, nvmax = ncol(data.lm)-1, method = "backward")
regfit.back.summary = summary(regfit.back)
which.max(regfit.back.summary$adjr2)
plot(regfit.back.summary$adjr2, col = c(rep(1,10), 2), pch = 19, cex = 2)
coef(regfit.back, 11)

#try forward methord
regfit.for = regsubsets(Count~., data = data.lm, nvmax = ncol(data.lm)-1, method = "forward")
regfit.for.summary = summary(regfit.for)
which.max(regfit.for.summary$adjr2)
plot(regfit.for.summary$adjr2, col = c(rep(1,12), 2), pch = 19, cex = 2)
coef(regfit.for, 12)
#It seems linear regression doesn't fit well, try logistic regression


##################################################################
#### Part 3: Classification--Predict whether exceed capacity #####
##################################################################

########################################
###########logistic regression

#find the capacity of each station from station.csv and add the capacity as "dockcount"
count = c()
for (i in start.data$from_station_name){
  count = append(count, station[which(station == i)-58, 8])
}
start.data$dockcount = as.integer(count)
#creat a new variable "is_exceed", indicating if there are more user start from a station than the capacity
start.data$is_exceed = ifelse(start.data$Count > start.data$dockcount, 1, 0)
table(start.data$is_exceed)
#just remove all the missing values and select all the numrical except "count" colums as predictors
data.lg = na.omit(start.data[, c(6,9,12,15,18,21,23,25:39)])
#divide into training and testing
train = sample(1:nrow(data.lg), 0.9*nrow(data.lg))
data.lg.train = data.lg[train, ]
data.lg.test = data.lg[-train, ]
#run the logistic model and see the summary
lg.model = glm(is_exceed~., data = data.lg.train, family = binomial(link='logit'))
summary(lg.model)
#do predictio based on the logistic model
data.lg.test$lg_pred_prob = predict(lg.model, data.lg.test, "response")
data.lg.test$lg_pred_class = rep(1, nrow(data.lg.test))
data.lg.test$lg_pred_class[data.lg.test$lg_pred_prob <= .5] = 0
#confusion table
table(data.lg.test$lg_pred_class, data.lg.test$is_exceed)

#this accuracy is 94.7%

########################################
###########clasification tree
library(rpart)
library(rpart.plot)

ct_model<-rpart(as.factor(is_exceed)~., 
                data = data.lg.train,
                method="class",
                control=rpart.control(cp=0,maxdepth=10))
print(ct_model)  
summary(ct_model)

##use the model to do the prediction
data.lg.test$ct_pred_prob<-predict(ct_model,data.lg.test)[,2]
data.lg.test$ct_pred_class<-predict(ct_model,data.lg.test,type="class")

##See the results
table(data.lg.test$is_exceed==data.lg.test$ct_pred_class) 
#Confusion table
table(data.lg.test$ct_pred_class,data.lg.test$is_exceed, dnn=c("predicted","actual"))

### k-fold Cross-validation 
set.seed(1)   # set a random seed 
full_tree<-rpart(as.factor(is_exceed)~., 
                 data = data.lg.train,
                 method="class",
                 control=rpart.control(cp=0))
rpart.plot(full_tree)
# xerror, xstd - cross validation results 
printcp(full_tree)
plotcp(full_tree)

# prune tree with minimum cp value
min_xerror<-full_tree$cptable[which.min(full_tree$cptable[,"xerror"]),]
min_xerror
min_xerror_tree<-prune(full_tree, cp=min_xerror[1])
rpart.plot(min_xerror_tree)

#consider mim_xerror_tree as the best pruned tree, and get the prediction. 
bp_tree<-min_xerror_tree
data.lg.test$ct_bp_pred_prob<-predict(bp_tree,data.lg.test)[,2]
data.lg.test$ct_bp_pred_class=ifelse(data.lg.test$ct_bp_pred_prob>0.5,1,0)

table(data.lg.test$ct_bp_pred_class==data.lg.test$is_exceed)  # error rate
table(data.lg.test$ct_bp_pred_class,data.lg.test$is_exceed, dnn=c("predicted","actual"))  # confusion table on test data


########################################
###########Random Forest

library(randomForest)
rf.lg<-randomForest(as.factor(is_exceed)~., data = data.lg.train, importance = T, ntree = 300)
rf.lg
data.lg.test$rf_pred_prob<-predict(rf.lg,data.lg.test,type="prob")[,2]
data.lg.test$rf_pred_class<-predict(rf.lg,data.lg.test,type="class")
rf.lg.pred = predict(rf.lg, newdata = data.lg.test)
table(data.lg.test$rf_pred_class,data.lg.test$rf_pred_class, dnn=c("predicted","actual")) 
#the accuracy is aroutn 94.8% 

########################################
###########Performance Visualization with ROC
library(pROC)
lg_roc<-roc(data.lg.test$is_exceed,data.lg.test$lg_pred_prob,auc=TRUE)
plot(lg_roc,print.auc=TRUE,col="blue")

ct_roc<-roc(data.lg.test$is_exceed,data.lg.test$ct_bp_pred_prob,auc=TRUE)
plot(ct_roc,print.auc=TRUE,print.auc.y=.3, col="red",add=TRUE)

rf_roc<-roc(data.lg.test$is_exceed,data.lg.test$rf_pred_prob,auc=TRUE)
plot(rf_roc,print.auc=TRUE,print.auc.y=.4,col="green", add=TRUE)


###################################################
#### Part 3: Clustering--cluster the stations #####
###################################################

#read data, focuse on station this time
station = read.csv("data/station.csv")
start.data = read.csv("Data/predic_start.csv")
end.data = read.csv("Data/predic_end.csv")

###########################################
#######first use lng&lat to do kmeans

#select lng&lat colums for kmeans
station.kmeans.lng_lat = station[, c(3,4)]

#use elbow method to determin which is the best k
set.seed(1)
withinnss = c()
for (i in c(1:57)){
  km.out = kmeans(station.kmeans.lng_lat,i)
  withinnss = append(withinnss, km.out$withinss[1])
}
#plot the elbow curve and find out that the best k shoul be around 3~5
plot(withinnss, col = c(rep(1, 4), 2, rep(1, 52)), pch = 19, cex = 1)
#then us k=5 to do the kmeans again
station.kmeans.model.lng_lat = kmeans(station.kmeans.lng_lat,5,nstart=20)

#visulization kmeans results
library(leaflet)
df <- sp::SpatialPointsDataFrame(
  cbind(
    station.kmeans.lng_lat$long,  # lng
    station.kmeans.lng_lat$lat# lat
  ),
  data.frame(type = factor(
    station.kmeans.model.lng_lat$cluster,
    c(1,2,3,4,5)
  ))
)
pal <- colorFactor(c("purple", "red", "black", "orange", "green"), domain = c(1,2,3,4,5))
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = 6,
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 1
  )

########################################
###########use more variables to find something more interesting
#create two new variables: "exceed" & "not_exceed", indicating how many times a station recieve more bike than send out
exceed = c()
not_exceed = c()
for(i in station$name){
  c.1 = 0
  c.2 = 0
  temp.start = start.data[start.data$from_station_name == i,]
  temp.end = end.data[end.data$to_station_name == i,]
  for (j in temp.start$Date){
    if (j %in% temp.end$Date){
      if(temp.start[temp.start$Date == j,]$Count-temp.end[temp.end$Date == j,]$Count >=0){
        c.1 = c.1+1
      }else{
        c.2 = c.2+1
      }
    }
  }
  exceed = append(exceed, c.1)
  not_exceed = append(not_exceed, c.2)
}
#if out > in, it is exceed
station$exceed = exceed
#if out < in, it is not exceed
station$not_exceed = not_exceed
#if a station has a modification date, that means it has been modified
station$modified = ifelse(station$modification_date == '', 0, 1)
#if a station has a decommission date, that means it no longer in use
station$in_use = ifelse(station$decommission_date == '', 1, 0)

#we will use 6 variables other than lng&lat to do the kmeans again, hope we can find some interesting results
station.kmeans.new = station[, c(6,8,10,11,12,13)]

#elbow method to determine k
withinnss = c()
for (i in c(1:57)){
  km.out = kmeans(station.kmeans.new,i)
  withinnss = append(withinnss, km.out$withinss[1])
}
#draw the elbow curve. From the graph, best k should be 4~6
plot(withinnss, col = c(rep(1, 5), 2, rep(1, 51)), pch = 19, cex = 1)
#again, use k=5 to do the kmeans and to see what's the different with last one
station.kmeans.model = kmeans(station.kmeans.new,5,nstart=20)

#visulization the clustering results
library(leaflet)
df <- sp::SpatialPointsDataFrame(
  cbind(
    station.kmeans.lng_lat$long,  # lng
    station.kmeans.lng_lat$lat# lat
  ),
  data.frame(type = factor(
    station.kmeans.model$cluster,
    c(1,2,3,4,5)
  ))
)
pal <- colorFactor(c("purple", "red", "black", "orange", "green"), domain = c(1,2,3,4,5))
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = 6,
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 1
  )