# Week 7 Homework 2: 
#
# * Sat Nov 23 2013 08:16:55 PM Steven E. Pav <steven@cerebellumcapital.com>
#


fdisp <- function(x) {
	cat("# ",x,"\n")
}

# really. https not supported? jeezus.
really.read.delim <- function(url,...) {
	temporaryFile <- tempfile()
	download.file(url,destfile=temporaryFile, method="curl")
	retval <- read.table(temporaryFile,...)
}

fb.url <- 'https://d396qusza40orc.cloudfront.net/assetpricing%2Fdata%2FFama_Bliss_price.txt'
fbliss <- really.read.delim(fb.url,
											 skip=2,
											 check.names=FALSE,
											 col.names=c("date","price1","price2","price3","price4","price5"))


require(xts)
TEO <- as.Date(as.character(fbliss$date),"%Y%m%d")
TEO <- as.POSIXct(TEO)
fxts <- xts(fbliss[,!(colnames(fbliss) %in% "date")],order.by=TEO)

# all Q's start in 1964...
fxts <- fxts["1964-01-30::2013-06-27",]

# ok, finally. 
# divide by 100, then take the log
log.p <- log((1/100) * fxts)

# compute the yields
yield <- -log.p / col(fxts)

# the forward prices
forward <- t(- apply(t(log.p),2,diff))

# the returns
rets <- log.p[13:nrow(log.p),1:(ncol(log.p)-1)] - as.matrix(log.p[1:(nrow(log.p)-12),2:(ncol(log.p))])

# excess returns
rets.x <- rets - rep(as.matrix(yield[1:(nrow(rets)),1]),ncol(rets))

# Question 1#FOLDUP
# this neglects sweeping the means out. duh!
decomp <- svd(as.matrix(rets.x))
fdisp(decomp$d)
#  WRONG: 1.423 0.4386 0.266 0.2296 

dets.x <- sweep(as.matrix(rets.x), 2, apply(as.matrix(rets.x), 2, mean))

# hmm. ok.
Sigma <- cov(rets.x)
Q <- eigen(Sigma)
fdisp(100 * sqrt(Q$values))
#UNFOLD

# Question 2#FOLDUP
decomp <- svd(as.matrix(rets.x),nv=1)
fdisp(decomp$v)
#  0.213 0.4022 0.5637 0.6893 
#UNFOLD

# Question 3#FOLDUP
resu <- NULL
decomp <- svd(as.matrix(rets.x))
xv <- as.matrix(decomp$u[,1])

for (iii in c(1:4)) {
	yv <- as.matrix(rets.x[,iii])
	amod <- lm(yv ~ xv)
	amod.s <- summary(amod)
	resu <- c(resu,amod.s$r.squared)
}
xv <- as.matrix(decomp$u[,(1:2)],nrow=dim(rets.x)[1])
for (iii in c(1:4)) {
	yv <- as.matrix(rets.x[,iii])
	amod <- lm(yv ~ xv)
	amod.s <- summary(amod)
	resu <- c(resu,amod.s$r.squared)
}
fdisp(resu)
#  0.9216 0.9799 0.9972 0.9929 0.9883 0.9976 0.9975 0.9995 
#UNFOLD

# Question 4#FOLDUP
decomp <- svd(as.matrix(rets.x))
plot(decomp$v[,1])

# Unlike yields, the first factor loadings rise almost linearly with maturity. The second factor has a curved as well as slope shape. Factors three and four are tiny without economic pattern. The two factors together account for 99% of the variance of returns.

#UNFOLD

# Question 5#FOLDUP
a <- 0.06
rho <- 0.95
phi <- 0.60

evl <- function(nn,a=0.06,rho=0.95,phi=0.60) {
	#resu <- a + rho^(nn-1) + phi^(nn-1)
	resu <- c(rho^(nn-1),phi^(nn-1))
}

f2 <- evl(2)
f3 <- evl(3)

fdisp(c(f2,f3))
#  0.95 0.6 0.9025 0.36 

#UNFOLD

# Question 6#FOLDUP
a <- 0.06
rho <- 0.95
phi <- 0.60

evl <- function(nn,a=0.06,rho=0.95,phi=0.60) {
	rp <- function(fc) {
		(1 - fc^(nn)) / (1-fc) 
	}
	resu <- c(rp(rho),rp(phi)) / nn
}

y2 <- evl(2)
y3 <- evl(3)

fdisp(c(y2,y3))
#  0.975 0.8 0.9508 0.6533 
#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
