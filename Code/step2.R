
#kmeans implementation before distortion analysis
fit <- kmeans(crime0,4)
 fit
#kmeans description
#K-means clustering with 4 clusters of sizes 3, 4, 1, 23

#Cluster means:
#  ANTI.SOCIAL.BEHAVIOUR BICYCLE.THEFT   BURGLARY CRIMINAL.DAMAGE.AND.ARSON      DRUGS
#1            303.000000    3.33333333  65.333333                 86.333333 16.0000000
#2            183.250000    1.75000000  39.500000                 41.750000  8.2500000
#3           1052.000000   56.00000000 182.000000                247.000000 77.0000000
#4              3.478261    0.04347826   1.478261                  1.391304  0.2173913
#OTHER.CRIME OTHER.THEFT POSSESSION.OF.WEAPONS PUBLIC.ORDER   ROBBERY SHOPLIFTING
#1   6.6666667   55.666667              4.666667  12.00000000  2.333333  65.0000000
#2   8.2500000   31.750000              2.250000   6.25000000  1.750000  24.5000000
#3  27.0000000  227.000000             15.000000  50.00000000 38.000000 214.0000000
#4   0.6956522    0.826087              0.000000   0.08695652  0.000000   0.5652174
#THEFT.FROM.PERSON VEHICLE.CRIME VIOLENCE.AND.SEXUAL.OFFENCES
#1        6.33333333     58.000000                   112.000000
#2        1.75000000     50.000000                    71.500000
#3       36.00000000    245.000000                   430.000000
#4        0.04347826      3.652174                     1.565217

#Clustering vector:
#  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 
#1  4  4  4  4  2  4  4  1  3  4  4  1  4  2  4  4  4  4  4  2  4  4  4  4  4  2  4  4  4  4 

#Within cluster sum of squares by cluster:
 # [1]  4215.333  4730.500     0.000 12025.304
#(between_SS / total_SS =  98.8 %)

#Available components:
  
#  [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
#[7] "size"         "iter"         "ifault"      
#> fit$withinss
#[1]  4215.333  4730.500     0.000 12025.304
#> fit$betweenss
#[1] 1777147
#> fit$size 
#[1]  3  4  1 23


 table(data.frame(crime0$State, fit$cluster))


#optimized K value by Elbow Curve
kmeans.wss.k <- function(crime, k) {
  km = kmeans(crime, k)
  return (km$tot.withinss)
}
maxk=10
kmeans.dis <- function(crime, maxk){
        dis=(nrow(crime)-1)*sum(apply(crime,2,var))
        dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime=crime)
        return(dis)
}  

#distortion function
dis = kmeans.dis(crime0, maxk);
 
    plot(1:maxk, dis, type='b', xlab="Number of Clusters",
                   ylab="Distortion",
                  col="blue")



