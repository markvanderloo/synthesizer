# Check whether classes that corresponds with spatial clusters
# are retained.

set.seed(1)
# Create a dataset with separate clusters based on numeric data
# and a classifying variable that coincides with the cluster
# locations

# centers
centers <- list(
    cl1 = list(x=-10,    y= -10,col="black")
  , cl2 = list(x=-10,    y=  10,col="red")
  , cl3 = list(x= 10,    y= -10,col="blue")
  , cl4 = list(x= 10,    y=  10,col="green")
)
# create n records per cluster
n <- 100
L <- lapply(centers, function(center){
  data.frame(  x = rnorm(n=n, mean=center$x)
             , y = rnorm(n=n, mean=center$y)
             , col = center$col)
})

clusters <- do.call("rbind",L)
clusters1 <- clusters[sample(4*n),,drop=FALSE]

# synthesize de whole dataset
synth_clusters  <- synthesize(clusters)
synth_clusters1 <- synthesize(clusters1)

real_mean    <- aggregate(clusters[1:2], by = clusters[3], mean)
synth_mean   <- aggregate(synth_clusters[1:2], by=synth_clusters[3], mean)
synth_mean1  <- aggregate(synth_clusters1[1:2], by=synth_clusters1[3], mean)

expect_equivalent(synth_mean, real_mean, tolerance=0.25)
expect_equivalent(synth_mean1, real_mean, tolerance=0.25)

