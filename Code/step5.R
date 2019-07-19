#my own kmeans algorithms
CRIMEDATASET <- read.csv("C:/Users/HARISH/Desktop/SEM 7/CIP/datasets/CRIMEDATASET.csv")
View(CRIMEDATASET)
euclid <- function(points1, points2) {
      distanceMatrix <- matrix(NA, nrow=dim((points1))[1], ncol=dim((points2))[1])
      distanceMatrix
      for(i in 1:nrow(points2)) {
            distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
            distanceMatrix[,i]
       }
     distanceMatrix
   }
 K_means <- function(x, centers, distFun, nItter) {
       clusterHistory <- vector(nItter, mode="list")
       centerHistory <- vector(nItter, mode="list")
      
         for(i in 1:nItter) {
               distsToCenters <- distFun(x, centers)
               clusters <- apply(distsToCenters, 1, which.min)
              centers <- apply(x, 2, tapply, clusters, mean)
              # Saving history
                clusterHistory[[i]] <- clusters
               centerHistory[[i]] <- centers
           }
       
        list(clusters=clusterHistory, centers=centerHistory)
   }
 test=CRIMEDATASET# A data.frame
 test = CRIMEDATASET[,2:ncol(CRIMEDATASET)]
 ktest=as.matrix(test) # Turn into a matrix
 centers <- ktest[sample(nrow(ktest), 5),] # Sample some centers, 5 for example
 res <- K_means(ktest, centers, euclid, 10)
 
 