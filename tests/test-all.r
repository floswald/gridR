
library(testthat)

x <- runif(30)
b <- 10
deg <- 5
context("testing knot.select")
test_that( "output of knot.select " ,{
		  expect_that( length(knot.select(degree=deg,x=x,num.basis=b)) == b + deg + 1, is_true())
		  expect_that( class(knot.select(degree=deg,x=x,num.basis=b)) == "knotVec", is_true())
		  expect_that( attr(knot.select(degree=deg,x=x,num.basis=b),"num.basis") == b, is_true())
		  expect_that( knot.select(degree=deg,x=x,num.basis=deg), gives_warning())
})
		  

b <- c(-3,10)
n <- 30

context("testing gridR")
test_that( "test length of each grid.maker" ,{
		  expect_that( length(grid.maker(bounds=b,num.points=n,spacing="log.g")) == n, is_true())
		  expect_that( length(grid.maker(bounds=b,num.points=n,spacing="log.g2")) == n, is_true())
		  expect_that( length(grid.maker(bounds=b,num.points=n,spacing="hyp.sine")) == n, is_true())
		  expect_that( length(grid.maker(bounds=b,num.points=n,spacing="exp.grid")) == n, is_true())
		  expect_that( length(grid.maker(bounds=b,num.points=n,spacing="gumbel.grid",loc=1,scale=1.1)) == n, is_true())
		  expect_that( length(grid.maker(bounds=b,num.points=n,spacing="lognorm.grid",meanlog=2,sdlog=0.7)) == n, is_true())
		  expect_that( length(grid.maker(bounds=b,num.points=n,spacing="beta.grid",shape1=2,shape2=5)) == n, is_true())
})

test_that( "test output of each grid.maker is within bounds" ,{
		  expect_that( all.equal(grid.maker(bounds=b,num.points=n,spacing="log.g")[c(1,n)], b), is_true())
		  expect_that( all.equal(grid.maker(bounds=b,num.points=n,spacing="log.g2")[c(1,n)], b), is_true())
		  expect_that( all.equal(grid.maker(bounds=b,num.points=n,spacing="hyp.sine")[c(1,n)], b), is_true())
		  expect_that( all.equal(grid.maker(bounds=b,num.points=n,spacing="exp.grid")[c(1,n)], b), is_true())
		  expect_that( all.equal(grid.maker(bounds=b,num.points=n,spacing="gumbel.grid",loc=1,scale=1.1)[c(1,n)], b), is_true())
		  ln <- grid.maker(bounds = b, num.points = n, spacing = "log.g")[c(1, n)]
		  expect_that( all.equal(c(max(ln[1],0),ln[2]), c(max(b[1],0),b[2])), is_true())
		  expect_that( all.equal(grid.maker(bounds=b,num.points=n,spacing="beta.grid",shape1=2,shape2=5)[c(1,n)], b), is_true())
})

test_that( "test class of each grid.maker" ,{
		  expect_that( class(grid.maker(bounds=b,num.points=n,spacing="log.g")) == "gridR", is_true())
		  expect_that( class(grid.maker(bounds=b,num.points=n,spacing="log.g2")) == "gridR", is_true())
		  expect_that( class(grid.maker(bounds=b,num.points=n,spacing="hyp.sine")) == "gridR", is_true())
		  expect_that( class(grid.maker(bounds=b,num.points=n,spacing="exp.grid")) == "gridR", is_true())
		  expect_that( class(grid.maker(bounds=b,num.points=n,spacing="gumbel.grid",loc=1,scale=1.1)) == "gridR", is_true())
		  expect_that( class(grid.maker(bounds=b,num.points=n,spacing="lognorm.grid",meanlog=2,sdlog=0.7)) == "gridR", is_true())
		  expect_that( class(grid.maker(bounds=b,num.points=n,spacing="beta.grid",shape1=2,shape2=5)) == "gridR", is_true())
})                                                     

test_that( "test attribute of each grid.maker" ,{
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="log.g"),"spacing") == "log.g", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="log.g2"),"spacing") == "log.g2", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="hyp.sine"),"spacing") == "hyp.sine", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="exp.grid"),"spacing") == "exp.grid", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="gumbel.grid",loc=1,scale=1.1),"spacing") == "gumbel.grid", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="lognorm.grid",meanlog=2,sdlog=0.7),"spacing") == "lognorm.grid", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="beta.grid",shape1=2,shape2=5),"spacing") == "beta.grid", is_true())
})                                                     

test_that( "test interface" ,{
		  expect_that( grid.maker(bounds=b,num.points=n,spacing="Spacing?"), gives_warning())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="log.g"),"spacing") == "log.g", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="log.g2"),"spacing") == "log.g2", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="hyp.sine"),"spacing") == "hyp.sine", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="exp.grid"),"spacing") == "exp.grid", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="gumbel.grid",loc=1,scale=1.1),"spacing") == "gumbel.grid", is_true())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="lognorm.grid",meanlog=2,sdlog=0.7),"spacing") == "lognorm.grid", is_true())
		  expect_that( grid.maker(bounds=c(-3,10),num.points=n,spacing="lognorm.grid",meanlog=2,sdlog=0.7), gives_warning())
		  expect_that( attr(grid.maker(bounds=b,num.points=n,spacing="beta.grid",shape1=2,shape2=5),"spacing") == "beta.grid", is_true())
})                              



