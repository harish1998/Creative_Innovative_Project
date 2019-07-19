
#importing the target data set
#CRIMEDATASET <- read.csv("C:/Users/user/Desktop/CRIMEDATASET.csv")
View(CRIMEDATASET)
crime<-CRIMEDATASET
#structure of data to preprocess it
str(crime)
summary(crime)
crime[!complete.cases(crime),]
dim(crime)
#Data cleaning
crime <- na.omit(crime,na.action=TRUE)
class(crime$STATES)

#outlier analysis
crime0<-crime[,c(2,3:15)]
 boxplot(crime0)
 boxplot(crime0[,c(2)])
 plot(crime0[,c(2)])
 #library(data.table)
 #outlier treatment
 outlierReplace = function(dataframe, cols, rows, newValue = NA){ 
                 if (any(rows)) {
                          set(dataframe, rows, cols, newValue)
                       }
 }
outlierReplace(crime0, "ANTISOCIALBEHAVIOUR", which(crime0$ANTI.SOCIAL.BEHAVIOUR > 1052), 1052)
outlierReplace(crime0, "Bicycle Theft", which(crime0$BICYCLE.THEFT> 56), 56)
 
 #plots
 #plot(crime0$OTHER.CRIME)
 # wss <- (nrow(crime0)-1)*sum(apply(crime0,2,var))
 # for(i in 1:5)wss[i]<- sum(fit=kmeans(crime0,centers=i,5)$withinss)
# plot(1:5,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with cluster sum of squares")
 # cluster curve
 # without any noise, after cleaning, can we achieve it ?

 
 
 
 