
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


# test for 'data.frame'

expect_equal(dim(make_synthesizer(iris)(10)), c(10,5))
expect_equal(dim(make_synthesizer(iris)(10)), c(10,5))
expect_equal(dim(make_synthesizer(iris)(250)), c(250,5))



