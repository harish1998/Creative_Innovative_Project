
#Enhanced Kmeans with better initalization
kmeansp2 <- function(x, k, iter.max = 10, nstart = 1, ...) {
  n <- nrow(x) # number of data points
  centers <- numeric(k) # IDs of centers
  distances <- matrix(numeric(n * (k - 1)), ncol = k - 1) 
  res.best <- list(tot.withinss = Inf) 
  for (rep in 1:nstart) {
    pr <- rep(1, n) # probability for sampling centers
    for (i in 1:(k - 1)) {
      centers[i] <- sample.int(n, 1, prob = pr) 
      distances[, i] <- colSums((t(x) - x[centers[i], ])^2) 
      pr <- distances[cbind(1:n, max.col(-distances[, 1:i, drop = FALSE]))] 
    }
    centers[k] <- sample.int(n, 1, prob = pr)
    ## Perform k-means with the obtained centers
    res <- kmeans(x, x[centers, ], iter.max = iter.max, nstart = 1, ...)
    res$inicial.centers <- x[centers, ]
    ## Store the best result
    if (res$tot.withinss < res.best$tot.withinss) {
      res.best <- res
    }
  }
  res.best
}

kmeansp2(ktest, 4)


