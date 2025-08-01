
# test for 'numeric'
set.seed(1)
y <- rnorm(100)
p <- sapply(1:100, function(i) ks.test(make_synthesizer(y)(100),y)$p.value)
expect_true(mean(p) >= 0.8)

# test for 'integer'
y <- rpois(100, lambda=8)
# need to suppress warning because poisson is not really a continuous distribution.
# however, for integers, we approximate it with a continuous distribution and round
# afterwards. (So it is probably, sort of, ok)
p <- sapply(1:100, function(i) suppressWarnings(ks.test(make_synthesizer(y)(100),y)$p.value))
expect_true(mean(p) >= 0.8)

expect_true( all(is.integer(make_synthesizer(y)(50))) )


# test for 'character'
y <- sample(c("aap", "noot", "mies"), 100, replace=TRUE)
R <- make_synthesizer(y)
expect_true(is.character(R(100)))

p <- sapply(1:100, function(i) mean(table(y)-table(R(100))) )

expect_true(abs(mean(p)) <= 0.1)


# test for 'factor'
#
#
y <- sample(as.factor(c("aap", "noot", "mies")), 100, replace=TRUE)
R <- make_synthesizer(y)
expect_true(is.factor(R(100)))

p <- sapply(1:100, function(i) mean(table(y)-table(R(100))) )

expect_true(abs(mean(p)) <= 0.1)

# test for logical
y <- sample(c(TRUE, FALSE),100, replace=TRUE)
R <- make_synthesizer(y)
p <- sapply(1:100, function(i) mean(table(y)-table(R(100))) ) 
expect_true(abs(mean(p))<= 0.1)

# test that missing data is handled correctly
expect_silent(make_synthesizer(c(1,NA,2,NA,NA)))
expect_equal(synthesize(rep(NA_real_,3)),rep(NA_real_,3))
expect_equal(synthesize(rep(NA,3)),rep(NA,3))



# test correlation between NA and obserevd values
rl <-data.frame( x = c(rnorm(50,mean=10), rnorm(50,mean=-10))
               , y = c(rep(NA_real_,50), rnorm(50)))

sn <- synthesize(rl)
expect_true(mean(sn$x[is.na(sn$y)]) > mean(sn$x[!is.na(sn$y)]))


# test for 'data.frame'

expect_equal(dim(make_synthesizer(iris)(10)), c(10,5))
expect_equal(dim(make_synthesizer(iris)(10)), c(10,5))
expect_equal(dim(make_synthesizer(iris)(250)), c(250,5))

# test with lowered correlations
d <- data.frame(x=1:100, y=1:100, z=1:100)
s <- synthesize(d,rankcor=0.5)
expect_true(cor(d$x,s$x)<0.5)
expect_true(cor(d$y,s$y)<0.5)
expect_true(cor(d$z,s$z)<0.5)

s1 <- synthesize(d, rankcor=c("z"=0.5))
expect_true(cor(d$z,s$z)<=0.5)


# test na.rm 
d <- data.frame(
   x = rnorm(100)
 , y = sample(letters, 100,replace=TRUE)
 , z = as.factor(sample(letters,100,replace=TRUE))
 , v = as.integer(sample(1:100,100, replace=TRUE)))
A <- matrix(sample(c(TRUE, FALSE), 4*100, replace=TRUE),nrow=100)
d[A] <- NA
expect_equal(sum(is.na(synthesize(d,na.rm=TRUE))),0)


# regression test for tie handling (errors for versions < 0.4.1)

expect_silent(synthesize(data.frame(X=1:10,Y=rep(1,10)),rankcor=0.5))





