

expect_equal(pmse(iris,iris), 0)
expect_silent(pmse(iris,iris, model="rf"))
expect_equal(dmean(iris,iris),0)
expect_equal(dsd(iris, iris),0)
expect_equal(dcor(iris,iris),0)

expect_error(dsd(cars, iris), pattern="names")
expect_error(dsd(data.frame(x=1,y=1), data.frame(x=1,y=1L)), pattern="types")


expect_equal(dim(qa(iris,n=10)), c(10,4))



