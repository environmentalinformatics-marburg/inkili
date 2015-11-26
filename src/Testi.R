df <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6))
dat <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6))
apply(dat[,c('x','z')], 1, function(x) sum(x) )
rowSums(dat[,c('x','z')])
testFunc <- function(a, b) a + b
apply(dat[,c('x','z')], 1, function(x) testFunc(x[1],x[2]))
sapply(dat[,c('x','z')], function(x) testFunc(x[1],x[2]))
apply(dat[,c('x','z')], 1, function(y) testFunc(y['z'],y['x']))


