


# gridR tests

gridR(bounds=c(-1,5),num.points=10,spacing="expo",plotit=FALSE)
gridR(bounds=c(-1,5),num.points=10,spacing="expo",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="expo2",plotit=FALSE)
gridR(bounds=c(-1,5),num.points=10,spacing="hyp.sine",plotit=FALSE)
gridR(bounds=c(-10,10),num.points=20,spacing="hyp.sine",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=TRUE)
gridR(bounds=c(-1,5),num.points=10,spacing="log.grid",plotit=FALSE)
gridR(bounds=c(1,5),num.points=10,spacing="log.norm",plotit=TRUE,meanlog=3,sdlog=0.5)
gridR(bounds=c(-1,5),num.points=10,spacing="log.norm",plotit=FALSE,meanlog=3,sdlog=0.5)
gridR(bounds=c(-1,5),num.points=10,spacing="gumbel",plotit=TRUE,loc=1,scale=1.1)
gridR(bounds=c(-1,5),num.points=10,spacing="gumbel",plotit=FALSE,loc=1,scale=1.1)
gridR(bounds=c(-1,5),num.points=10,spacing="beta",plotit=TRUE,shape1=2,shape2=5,ncp=1)
gridR(bounds=c(-1,5),num.points=10,spacing="beta",plotit=FALSE,shape1=2,shape2=5,ncp=1)
