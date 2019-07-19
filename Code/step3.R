#graphical plots
plot(crime0,col=fit$cluster,pch=15)
 points(fit$centers,col=1:8,pch=3)
 library(cluster)
 
 library(fpc)
plotcluster(crime0,fit$cluster)
 points(fit$centers,col=1:8,pch=16)
clusplot(crime0, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
 mydata <- crime[,c(2,3:15)]
 mydata <- data.frame(mydata,fit$cluster)

#aggregated results
 cluster_mean <- aggregate(mydata[,1:8],by = list(fit$cluster),FUN = mean)
cluster_mean