getwd()
rm(list=ls()) # remove the all variables
install.packages("DT")
install.packages("dummies")
install.packages("Metrics")
install.packages("caret")
install.packages("gbm")
taxi <- read.csv("Taxi-cancellation-case.csv",stringsAsFactors=FALSE, header = TRUE)
attach(taxi)
taxi$row. <- NULL
#View(taxi) # Checking all the data1 
#DT::datatable(taxi) # Checking all the data2
################ Load the basic library #############################
library(dplyr) # library for a grammar of data manipulation
library(DT) # Data table 
library(dummies) # convert to dummy variable -> we do not use this library in this case
library(caret) # confusion matrix
library(gbm) # 
library(httr)
library(ggplot2) # Data visualization
################ Check the Data #####################################
attach(taxi)
summary(taxi)
names(taxi)
head(taxi)
str(taxi)
nrow(taxi)
ncol(taxi)
colnames(taxi)
dim(taxi)
hist(taxi$Car_Cancellation,main ="Car Cancellation Histogram", xlab="Car Cancellation")
################ Variable Handling ##################################
#Vehicle_model_id
table(taxi$vehicle_model_id)
aggregate(Car_Cancellation ~ vehicle_model_id, taxi, mean)
taxi$vehicle_avg_cancelation
taxi$vehicle_model_id[ taxi$vehicle_model_id %in% c(1,13,17,30,36,70,91) ] <- 100
table(taxi$vehicle_model_id)
taxi$vehicle_model_id[ taxi$vehicle_model_id %in% c(10,23,54,64,86,100) ] <- 101
table(taxi$vehicle_model_id)
length(taxi$vehicle_model_id)
colnames(taxi)
#Travel_type_id -> Dummy variable
table(taxi$travel_type_id)
length(taxi$travel_type_id)
taxi$travel_type_id
taxi$Longdistance <- ifelse(taxi$travel_type_id==1,1,0)
taxi$Longdistance
taxi$PointToPoint <- ifelse(taxi$travel_type_id==2,1,0)
taxi$PointToPoint
str(taxi)
#Package ID
colnames(taxi)
table(taxi$package_id,useNA = "ifany")
#Traditional booking
taxi$traditional_booking <- ifelse(online_booking == 0 & mobile_site_booking == 0,1,0)
#Find the columns where the NAs
colnames(taxi)[colSums(is.na(taxi))>0]
install.packages("naniar")
library(naniar) # library(naniar) -> Checking the missing values
#Handling the missing values
vis_miss(taxi)
gg_miss_var_cumsum(taxi)
gg_miss_which(taxi)
miss_var_summary(taxi)
taxi[complete.cases(taxi), ]
################ Checking the imbalanced dataset ##################################
#The percentage of Car Cancellation
Car_Cancellation.Frequency <- table(taxi$Car_Cancellation)
Car_Cancellation.Percent <- prop.table(table(taxi$Car_Cancellation)) * 100
Car_Cancellation.Table <- cbind(Car_Cancellation.Frequency, Car_Cancellation.Percent)
Car_Cancellation.Table
class(taxi$Car_Cancellation)
#taxi$Car_Cancellation <- as.factor(taxi$Car_Cancellation)
#class(taxi$Car_Cancellation)
#Check whether the factor is or not
taxi.factor <- taxi[ , sapply(taxi, is.factor)]
str(taxi.factor)
str(taxi)
#Extract the features from the date/time field
library(stringr) # string library
taxi$from_time <- str_split_fixed(taxi$from_date, " ",2)[,2] #Separate time from date
str_split_fixed(taxi$from_time, " ",2)
taxi$from_hour <- factor(str_split_fixed(taxi$from_time, ":", 2)[,1]) #Extract hour from time
taxi$from_hour
taxi$from_minute <- str_split_fixed(taxi$from_time, ":", 2)[,2] #Extract minute from time
taxi$from_minute
taxi$from_date_formated <- as.Date(str_split_fixed(taxi$from_date, " ",2)[,1], format = "%m/%d/%Y")
taxi$from_date_formated
taxi$from_year <- factor(format(taxi$from_date_formated, "%Y")) 
taxi$from_month <- factor(format(taxi$from_date_formated, "%m"))
taxi$from_month
#Extract year from date and convert it to a factor(categorical variable)
taxi$from_year
#Manhatan distance
taxi$distance <- NA
nonna <- which(!is.na(taxi$to_lat) & !is.na(taxi$to_long) & !is.na(taxi$from_lat) & !is.na(taxi$from_long))
taxi$to_lat <- as.numeric(taxi$to_lat)
taxi$to_long <- as.numeric(taxi$to_long)
taxi$from_lat <- as.numeric(taxi$from_lat)
taxi$from_long <- as.numeric(taxi$from_long)
taxi$distance[nonna] <-  sqrt((taxi$to_long[nonna] - taxi$from_long[nonna])^2
                       + (taxi$to_lat[nonna] - taxi$from_lat[nonna])^2)
taxi$distance[is.na(taxi$distance)] <- mean(taxi$distance, na.rm = T)
taxi$distance
# library(geosphere)
# df <- data.frame(taxi$from_long,taxi$from_lat,taxi$to_long,taxi$to_lat)
# df
# distance <- distGeo(df)
# distance
# taxi$distance
# Format "to_date"
date_notnull <- which(taxi$to_date != "")
date_notnull
taxi$to_date_formated <- taxi$from_date_formated
taxi$to_date_formated
taxi$to_date_formated[date_notnull]
taxi$to_date_formated[date_notnull] <- as.Date(paste0(taxi$to_date[date_notnull],origin = as.Date("1899-12-31")))
taxi$to_date_formated[date_notnull]
# use paste0() to paste without spaces between the characters
taxi$to_date_formated[date_notnull] - taxi$from_date_formated[date_notnull]
taxi$weekday <- weekdays(taxi$to_date_formated) #Extract day of the week from date
taxi$weekday
#Difference between booking and departure
taxi$booking_created <- strptime(taxi$booking_created, format = "%m/%d/%Y %H:%M")
taxi$booking_created
taxi$from_date <- strptime(taxi$from_date, format = "%m/%d/%Y %H:%M")
taxi$from_date
taxi$waitingTime <- difftime(taxi$from_date,taxi$booking_created, units = "mins")
taxi$waitingTime
mean(taxi$waitingTime)
taxi$waitingTime[which(is.na(taxi$waitingTime))] <- mean(taxi$waitingTime,na.rm = T)
taxi$waitingTime[which(taxi$waitingTime < 0)] <- 100
taxi$waitingTime
taxi$booking_created <- as.character(taxi$booking_created)
taxi$from_date <- as.character(taxi$from_date)
taxi$Car_Cancellation <- as.numeric(taxi$Car_Cancellation)
#Average cancellation times by vehicle
vehicles <- taxi %>% 
  group_by(vehicle_model_id) %>% 
  summarise(Count = length(vehicle_model_id), 
            Avg_cancelation = mean(Car_Cancellation, na.rm = T), 
            traditional = mean(traditional_booking, na.rm = T),
            online = mean(online_booking, na.rm = T),
            mobile_site_booking = mean(mobile_site_booking, na.rm = T),)
vehicles <- filter(vehicles, Count >= 50)  #Remove vehicles with <50 occurences in the dataset
vehicles                          
names(vehicles)[3]  <- "vehicle_avg_cancelation"  #rename 3rd collumn
head(vehicles)
#Add average cancelation per travel type
taxi <- merge(taxi, vehicles[,c("vehicle_model_id","vehicle_avg_cancelation")],
              by = "vehicle_model_id", all.x =T)
taxi
taxi$vehicle_avg_cancelation[which(is.na(taxi$vehicle_avg_cancelation))] <- mean(taxi$Car_Cancellation, na.rm = T)
rm(vehicles)
taxi$travel_type_id <- as.factor(taxi$travel_type_id)
travel_type <- taxi %>% group_by(travel_type_id) %>% summarise(Count = length(travel_type_id), 
                                                               Avg_cancelation = mean(Car_Cancellation,
                                                                                       na.rm = T),
                                                               traditional = mean(traditional_booking, na.rm = T),
                                                               online = mean(online_booking, na.rm = T),
                                                               mobile_site_booking = mean(mobile_site_booking, na.rm = T))
travel_type 
#Travel_type
names(travel_type)[3] <- "travelType_avg_cancelation"
taxi <- merge(taxi, travel_type[,c("travel_type_id","travelType_avg_cancelation")], by = "travel_type_id", all.x = T)
rm(travel_type)
taxi$from_hour
taxi
#Add average cancellation by hour
new <- taxi
new <- new[,-which(names(new) %in% c("from_date","to_date","booking_created"))]
new
hours <- new %>% 
  group_by(from_hour) %>% 
  summarise(Count = length(from_hour),
            Avg_cancelation = mean(Car_Cancellation, na.rm = T))
names(hours)[3] <- "hour_avg_cancellation" 
hours
taxi$from_hour
taxi <- merge(taxi, hours[,c("from_hour",
                             "hour_avg_cancellation")], 
              by = "from_hour", all.x = T)
rm(new,hours)
##########################################################################
###########################DATA VISUALIZATION#############################
##########################################################################
#let's seee how booking method influences cancellation
taxi$booking_method <- NA
taxi$booking_method[which(taxi$mobile_site_booking == 1)] <- "Mobile Site"
taxi$booking_method[which(taxi$online_booking == 1)] <- "Online"
taxi$booking_method[is.na(taxi$booking_method)] <- "Traditional"
method <- taxi %>% group_by(booking_method) %>% summarise(Cancellation = length(which(Car_Cancellation == 1))/length(Car_Cancellation))
#Bar plot that shows cancellation rate for each type of booking method
ggplot(method, aes(y = Cancellation, x = booking_method, fill = booking_method)) + 
  geom_bar(stat = "identity", position = "Stack") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_light() + 
  ggtitle("Cancellation rate by method of booking")
rm(method)
#Graph the Cancellation rate by distance and waiting time
taxi$waitingTime <- as.numeric(taxi$waitingTime)
taxi$waitingTime[which(taxi$waitingTime < 0)] <- 100
options(scipen=999)
ggplot(taxi,
       aes(x = distance/1000, y = waitingTime/3600, color = Car_Cancellation)) +
  geom_point(alpha = 0.15, size = 6.5) +
  theme_light() +
  ggtitle("Cancellation rate by distance and waiting time") +
  ylab("Waiting time (hours)") + xlab("Distance (Km)")
#Cancellation rate by travelType
travelType <- taxi %>% group_by(travel_type_id) %>% summarise(Cancellation = length(which(Car_Cancellation == 1))/length(Car_Cancellation))
ggplot(travelType, aes(y = Cancellation, x = travel_type_id,fill=travel_type_id)) + 
  geom_bar(stat = "identity", position = "Stack") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_light() + 
  ggtitle("Cancellation rate by day type of travel") + xlab("")
rm(travelType)
#Cancellation rate by month
month <- taxi %>% group_by(from_month) %>% summarise(Cancellation = length(which(Car_Cancellation == 1))/length(Car_Cancellation))
month
ggplot(month, aes(y = Cancellation, x = from_month)) + 
  geom_bar(stat = "identity", position = "Stack") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_light() + 
  ggtitle("Cancellation rate by month") + xlab("")
rm(month)
#Cancellation rate by day of week
day <- taxi %>% group_by(weekday) %>% summarise(Cancellation = length(which(Car_Cancellation == 1))/length(Car_Cancellation))
day$weekday  = as.character(day$weekday)
day$weekday = factor(day$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
day = day[order(day$weekday),]
ggplot(day, aes(y = Cancellation, x = weekday,fill=weekday)) + 
  geom_bar(stat = "identity", position = "Stack") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_light() + 
  ggtitle("Cancellation rate by day of the week") + xlab("")
rm(day)
#Distribution and cancellation rates by hour
hour <- taxi %>% group_by(from_hour) %>% summarise(Rides = length(from_hour), Rides_freq = length(from_hour)/nrow(taxi),  Cancellations = length(which(Car_Cancellation == 1)), Cancellations_freq = mean(Car_Cancellation == 1)) %>% ungroup()
hour$from_hour <- as.character(hour$from_hour)
hour$from_hour <- factor(hour$from_hour, levels = append(seq(6, 23),seq(0,5)))
hour <- hour[order(hour$from_hour),]
rides_hour <- ggplot(hour, aes(y = Rides_freq, x = from_hour)) + 
  geom_bar(stat = "identity", position = "Stack") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_light() + 
  ggtitle("Rides distribution by hour") + xlab("") + ylab("% of total rides")
rides_hour
canc_hour <- ggplot(hour, aes(y = Cancellations_freq, x = from_hour)) + 
  geom_bar(stat = "identity", position = "Stack") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_light() + 
  ggtitle("Cancellation rate by hour") + xlab("") + ylab("Average cancellation")
canc_hour
install.packages("cowplot")
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2'
plot_grid(rides_hour, canc_hour, nrow = 2)
rm(hour, canc_hour, rides_hour)
#Cancellation rate by type of car
car <- taxi %>% group_by(vehicle_model_id) %>% summarise(Rides = length(vehicle_model_id), Cancellations = sum(Car_Cancellation == 1), Cancellation_rate = mean(Car_Cancellation == 1)) %>% ungroup()
car <- filter(car, Rides >= 100) #Filter out cars with less than 100 rides
car$vehicle_model_id = factor(car$vehicle_model_id)
ggplot(car, aes(y = Cancellation_rate, x = vehicle_model_id)) + 
  geom_bar(stat = "identity", position = "Stack") +
  theme_light() + 
  ggtitle("Cancellation rate by vehicle (min 100 rides)") + xlab("vehicle id") + ylab("Average cancellation rate")
rm(car)
##########################################################################
###########################Feature Engineering############################
##########################################################################
taxi$from_lat[which(is.na(taxi$from_lat))] <- mean(taxi$from_lat,na.rm = T)
taxi$from_long[which(is.na(taxi$from_long))] <- mean(taxi$from_long,na.rm = T)
taxi$to_lat[which(is.na(taxi$to_lat))] <- mean(taxi$to_lat,na.rm = T)
taxi$to_long[which(is.na(taxi$to_long))] <- mean(taxi$to_long,na.rm = T)
from_lat
taxi$waitingTime <- as.numeric(as.character(taxi$waitingTime))
taxi$waitingTime[which(is.na(taxi$waitingTime))] <- mean(taxi$waitingTime, na.rm = T)
taxi$waitingTime[which(taxi$waitingTime < 0)] <- 100
taxi$waitingTime
# Delete the unimportant variables
taxi$package_id<- NULL
taxi$to_area_id<- NULL
taxi$from_area_id<- NULL
taxi$from_city_id<- NULL
taxi$to_city_id<- NULL
taxi$to_date<- NULL
taxi$from_date<- NULL
taxi$to_date_formated<- NULL
taxi$from_date_formated<- NULL
taxi$from_time<- NULL
taxi$weekday<- NULL
taxi$booking_created<-NULL
taxi$from_year<-NULL
taxi$travel_type_id <- NULL
taxi$traditional_booking <- NULL
taxi$user_id <- NULL
taxi$booking_method <- NULL
taxi$travelType_avg_cancelation<- NULL
taxi$vehicle_avg_cancelation<- NULL
taxi$from_lat <- NULL
taxi$from_long <- NULL
taxi$to_lat<- NULL
taxi$to_long<- NULL
#taxi$user_id <- as.character(taxi$user_id)
str(taxi)
taxi$from_hour <- as.numeric(taxi$from_hour)
taxi$from_minute <- as.numeric(taxi$from_minute)
taxi$vehicle_model_id <- as.factor(taxi$vehicle_model_id)
str(taxi)
table(taxi$Car_Cancellation)
prop.table(table(taxi$Car_Cancellation))
# Handling the imbalanced dataset
# Balanced data set with both over and under sampling
#install.packages("ROSE")
# library(ROSE)
# taxi_balance <- ovun.sample(Car_Cancellation ~ ., data = taxi, method="over", N=18514)$data
#SMOTE
library(DMwR)
class(Car_Cancellation)
taxi$Car_Cancellation <- as.factor(taxi$Car_Cancellation)
str(taxi)
taxi_balance <- SMOTE(Car_Cancellation ~ ., data=taxi, perc.over = 500,perc.under=200)
table(taxi_balance$Car_Cancellation)
prop.table(table(taxi_balance$Car_Cancellation))
str(taxi_balance)
taxi_balance.df <- select(taxi_balance,-c(2,5,9))
cor(taxi_balance.df)
cor_taxi <- round(cor(taxi_balance.df),2)
cor_taxi[upper.tri(cor_taxi)]
set.seed(123) 
train.index <- sample(c(1:dim(taxi_balance)[1]), dim(taxi_balance)[1]*0.7)  
valid.index <- setdiff(c(1:dim(taxi_balance)[1]), train.index)
train.df <- taxi_balance[train.index, ]
valid.df <- taxi_balance[valid.index, ]
head(valid.df)
vis_miss(taxi_balance)
str(taxi_balance)
##########################################################################
#############################Build the model##############################
##########################################################################
# Build the Logistic model
options(scipen=999)
Car_Cancellation
m1 <- glm(Car_Cancellation ~., data=train.df,family='binomial')
summary(m1)
m1
#install.packages("coefplot")
# library(coefplot)
# coefplot(m1)
#coef(m1)
#calculate the probability of each customer using valid.df
#predict the probability and compare the probabiliy wih a cutoff value to decide 
#how to classify each observation into 1 or 0.
p_prob <- predict(m1, valid.df, type="response")
head(p_prob)
hist(p_prob)
data.frame(valid.df$Car_Cancellation,p_prob)[1:40,]
#set cutoff=0.5
pclass <- ifelse(p_prob>=0.5,1,0)
data.frame(valid.df$Car_Cancellation,p_prob,pclass)[1:40,]
#confusion matrix
cm <- table(pclass,valid.df$Car_Cancellation,
            dnn=c("prediction","Actual"))
cm
cm[1,2]
cm[2,1]
n <- nrow(taxi)
n1 <- floor(n*0.7)
error <- (cm[1,2]+cm[2,1])/(n-n1)
error
1-error
sensitivity <- cm[2,2]/(cm[1,2]+cm[2,2])
sensitivity
specificity <- cm[1,1]/(cm[1,1]+cm[2,1])
specificity
#False Positive
#False Negative
#Alternative way of getting confusion matrix
#install.packages("caret")
# install packages("ralang") #additional package for instructor computer
library("caret")
#install.packages('caret', dependencies = TRUE)
confusionMatrix(factor(pclass),factor(valid.df$Car_Cancellation),
                positive = "1") # positive: change the important factor
#set cutoff=0.4
pclass <- ifelse(p_prob>=0.4,1,0)
data.frame(valid.df$Car_Cancellation,p_prob,pclass)[1:40,]
#confusion matrix
cm <- table(pclass,valid.df$Car_Cancellation,
            dnn=c("prediction","Actual"))
cm
cm[1,2]
cm[2,1]
n <- nrow(taxi)
n1 <- floor(n*0.7)
error <- (cm[1,2]+cm[2,1])/(n-n1)
error
1-error
sensitivity <- cm[2,2]/(cm[1,2]+cm[2,2])
sensitivity
specificity <- cm[1,1]/(cm[1,1]+cm[2,1])
specificity
#False Positive
#False Negative
#Alternative way of getting confusion matrix
#install.packages("caret")
# install packages("ralang") #additional package for instructor computer
library("caret")
#install.packages('caret', dependencies = TRUE)
confusionMatrix(factor(pclass),factor(valid.df$Car_Cancellation),
                positive = "1") # positive: change the important factor
#set cutoff=0.3
pclass <- ifelse(p_prob>=0.3,1,0)
data.frame(valid.df$Car_Cancellation,p_prob,pclass)[1:40,]
#confusion matrix
cm <- table(pclass,valid.df$Car_Cancellation,
            dnn=c("prediction","Actual"))
cm
cm[1,2]
cm[2,1]
n <- nrow(taxi)
n1 <- floor(n*0.7)
error <- (cm[1,2]+cm[2,1])/(n-n1)
error
1-error
sensitivity <- cm[2,2]/(cm[1,2]+cm[2,2])
sensitivity
specificity <- cm[1,1]/(cm[1,1]+cm[2,1])
specificity
#False Positive
#False Negative
#Alternative way of getting confusion matrix

# Build the Decision Tree
library(rpart)
library(rpart.plot)
set.seed(1234)
cv.ct <- rpart(Car_Cancellation ~ .,data=train.df,
               method="class",cp=0.00001,xval=5)
# summary(cv.ct)
prp(cv.ct,type=1,extra=2,split.font = 1,varlen = -10, box.col=ifelse(cv.ct$frame$var=="<leaf>",'grey','white'))
###Decision Tree###
dt <- predict(cv.ct,valid.df,type="class")
confusionMatrix(factor(dt),factor(valid.df$Car_Cancellation),
                positive="1")
###Best prune Tree###
printcp(cv.ct)
plotcp(cv.ct)
min.xerror.cp <- cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"]
min.xerror.cp
0.48006+0.011255 # 0.491315
best.pruned.ct <- prune(cv.ct,cp=0.001366559)
prp(best.pruned.ct,type=1,extra=2,split.font = 1,varlen = -10)
pred <- predict(best.pruned.ct,valid.df,type="class")
confusionMatrix(factor(pred),factor(valid.df$Car_Cancellation),
                positive="1")
#install.packages("caret")
# install packages("ralang") #additional package for instructor computer
library("caret")
#install.packages('caret', dependencies = TRUE)
confusionMatrix(factor(pclass),factor(valid.df$Car_Cancellation),
                positive = "1") # positive: change the important factor
#install.packages("rattle")
#library(rattle)
#fancyRpartPlot(cv.ct)
# Using varImp() function
library(caret)
#Import the random forest library and fit a model
library(randomForest)
# Create an importance based on mean decreasing gini
rf <- randomForest(as.factor(Car_Cancellation)~.,data=train.df,ntree=500,
                   mtry=4,nodesize=5,importance=TRUE)
# compare the feature importance with varImp() function
varImp(rf)
# Create a plot of importance scores by random forest
varImpPlot(rf,type=2)
#predction evaluation
rf.pred <- predict(rf,valid.df)
library(caret)
confusionMatrix(as.factor(rf.pred),as.factor(valid.df$Car_Cancellation),
                positive = "1")
#boosted tree
#Generalized Boosted Regression Modeling (GBN)
#install.packages("gbm")
# library(gbm)
# set.seed(123)
# boost_tree <- gbm(Car_Cancellation~.,data=train.df,n.tree=500,
#                   distribution = "bernoulli") 
# boost_tree
# #if regression, distribution ="gaussian"
# summary(boost_tree, cBars=11,las=2,method=relative.influence)
# #install.packages("vip")
# library(vip)
# vip(boost_tree)
# #prediction based on boosted tree
# pred_boost <- predict(boost_tree,valid.df,n.trees = 500, type = "response")
# pred_boost 
# predict_class <- ifelse(pred_boost>0.2,1,0)
# predict_class
# confusionMatrix(as.factor(predict_class),as.factor(valid.df$Car_Cancellation),positive = "1")

# install.packages("BCA", dependencies = TRUE)
# install.packages("RcmdrMisc")
# library(BCA)
# lift.chart(c("rf.pred", "pred_boost"), data=valid.df, targLevel="Yes",
#             trueResp=0.01, type="cumulative", sub="")
cm_lg <- confusionMatrix(factor(pclass),factor(valid.df$Car_Cancellation),
                         positive = "1") 
cm_tree <- confusionMatrix(factor(pred),factor(valid.df$Car_Cancellation),
                           positive="1")
cm_rf <- confusionMatrix(as.factor(rf.pred),as.factor(valid.df$Car_Cancellation),
                  positive = "1")
model_compare <- data.frame(Model = c('Logistic Regression',
                                     'Decision Tree',
                                     'Random Forest'),
                           Accuracy = c(cm_lg$overall[1],
                                        cm_tree$overall[1],
                                        cm_rf$overall[1]))

ggplot(aes(x=Model, y=Accuracy), data=model_compare) +
  geom_bar(stat='identity', fill = 'blue') +
  ggtitle('Comparative Accuracy of Models') +
  xlab('Models') +
  ylab('Overall Accuracy')+
  coord_cartesian(ylim=c(0.00,0.95))

#install.packages('pROC')
# ROC for logistic
library(pROC) 
r1 <- roc(valid.df$Car_Cancellation,p_prob)
p_prob
plot.roc(r1,print.auc=TRUE,auc.polygon = TRUE)
auc(r1)

# ROC for decision tree 
dt <- predict(best.pruned.ct,valid.df,type = "prob")
r2 <- roc(valid.df$Car_Cancellation,dt[, 2])
plot.roc(r2,print.auc=TRUE,auc.polygon = TRUE)
auc(r2)

# ROC for random forest 
rt <- predict(rf,valid.df,type = "prob")
r3 <- roc(valid.df$Car_Cancellation,rt[, 2])
plot.roc(r3,print.auc=TRUE,auc.polygon = TRUE)
auc(r3)

# # ROC for boosted forest 
# bt <- predict(boost_tree,valid.df,n.trees = 500, type = "response")
# bt
# r4 <- roc(valid.df$Car_Cancellation,bt)
# plot.roc(r4,print.auc=TRUE,auc.polygon = TRUE)
# auc(r4)

# load package gains, compute gains (we will use package caret for categorical y later) 
library(gains)
#install.packages("gains")
Car_Cancellation
valid.df$Car_Cancellation <- as.numeric(valid.df$Car_Cancellation)
gain <- gains(valid.df$Car_Cancellation[!is.na(p_prob)], p_prob[!is.na(p_prob)]) 
# cumulative lift chart for logistic regression
options(scipen=999)
# avoid scientific notation 
# we will compute the gain relative to cancellation
cancellation <- valid.df$Car_Cancellation[!is.na(valid.df$Car_Cancellation)] 
plot(c(0,gain$cume.pct.of.total*sum(cancellation))~c(0,gain$cume.obs), xlab="# cases", 
     ylab="Cumulative Cancellation", main="Lift Chart", type="l") 
# baseline 
lines(c(0,sum(cancellation))~c(0,dim(valid.df)[1]), col="gray", lty=2) 
# Decile-wise lift chart 
barplot(gain$mean.resp/mean(cancellation), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")

#gain for decision tree
d_prob <- dt[, 2]
gain2 <- gains(valid.df$Car_Cancellation[!is.na(d_prob)], d_prob[!is.na(d_prob)]) 
gain2
# cumulative lift chart 
options(scipen=999) 
# avoid scientific notation 
# we will compute the gain relative to cancellation
cancellation <- valid.df$Car_Cancellation[!is.na(valid.df$Car_Cancellation)] 
plot(c(0,gain$cume.pct.of.total*sum(cancellation))~c(0,gain$cume.obs), xlab="# cases", 
     ylab="Cumulative Cancellation", main="Lift Chart", type="l") 
# baseline 
lines(c(0,sum(cancellation))~c(0,dim(valid.df)[1]), col="gray", lty=2) 
# Decile-wise lift chart 
barplot(gain$mean.resp/mean(cancellation), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")
# #gain for random forest
r_prob <- rt[,2]
gain <- gains(valid.df$Car_Cancellation[!is.na(r_prob)], r_prob[!is.na(r_prob)]) 
# gain
# cumulative lift chart 
options(scipen=999) 
# avoid scientific notation 
# we will compute the gain relative to cancellation
cancellation <- valid.df$Car_Cancellation[!is.na(valid.df$Car_Cancellation)] 
plot(c(0,gain$cume.pct.of.total*sum(cancellation))~c(0,gain$cume.obs), xlab="# cases",
     ylab="Cumulative Cancellation", main="Lift Chart", type="l") 
# baseline 
lines(c(0,sum(cancellation))~c(0,dim(valid.df)[1]), col="gray", lty=2) 
# Decile-wise lift chart 
barplot(gain$mean.resp/mean(cancellation), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")
#install.packages("BCA")
#library(BCA)
#library(Rcmdr)
#valid.df$Car_Cancellation <- as.factor(valid.df$Car_Cancellation)
#lift.chart(c("m1","cv.ct"),data=valid.df,targLevel=1,trueResp=0.01, type="cumulative", sub="Validation")

