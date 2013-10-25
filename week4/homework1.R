# Week 4 Homework 1: Mean Variance Frontier
#
# * Fri Oct 25 2013 09:41:56 AM Steven E. Pav <steven@cerebellumcapital.com>


# Question 1
#
# two assets with mean 

fcat <- function(x) {
	cat("# ",x,"\n")
}

really.read.delim <- function(url,...) {
	temporaryFile <- tempfile()
	download.file(url,destfile=temporaryFile, method="curl")
	retval <- read.table(temporaryFile,...)
}

# Question 3:#FOLDUP
ff3 <- really.read.delim("https://d396qusza40orc.cloudfront.net/assetpricing%2Fdata%2Fff_factors.txt",
											 skip=7,
											 check.names=FALSE,
											 col.names=c("date","Mkt.RF","SMB","HML","RF"))
head(ff3)

ff25 <- really.read.delim("https://d396qusza40orc.cloudfront.net/assetpricing%2Fdata%2Fff_25.txt",
													comment.char='%',
													check.names=FALSE)
colnames(ff25) <- c("date",sapply(1:25,function(n) { sprintf("port_%02d",n) })) 
cutd <- 194700
subf <- ff3[ff3$date > cutd,]
sub25 <- ff25[ff25$date > cutd,]
Re <- sub25[,2:26] - subf$RF

Sigma <- cov(Re)
mu <- colMeans(Re)
dim(mu) <- c(25,1)
marko <- solve(Sigma,mu)

mumax <- t(marko) %*% mu
sgmax <- sqrt(t(marko) %*% Sigma %*% marko)
srmax <- mumax / sgmax
	
fcat(c(100 * mumax,100 * sgmax,srmax)) 
#  15.27 39.07 0.3907 

# just checking
require(SharpeR)

srfoo <- as.sropt(sub25[,2:26],ope=1,epoch="month") 
#UNFOLD

# Question 4#FOLDUP
Sigma <- cov(Re)
mu <- colMeans(Re)
SM <- Sigma + mu %*% t(mu)
Wre <- solve(SM,mu)
dim(Wre) <- c(25,1)
Restar <- as.matrix(Re) %*% Wre
plot(Restar)
plot(cumprod(1+ 0.1 * Restar))

mustar <- mean(Restar)
sgstar <- sd(Restar)
srstar <- mustar / sgstar
fcat(c(100 * mustar,100 * sgstar,srstar)) 
#  13.25 33.9 0.3907 

#UNFOLD

# Question 5#FOLDUP
# the semantics of FF25 are value 1 through 5 repeated
# then cap 1 through 5.
value <- rep(1:5,5)
cap <- 1:5 %x% rep(1,5)

foomod <- lm(marko ~ value + cap)
print(summary(foomod))

# greater weightings based on value, lesser based on
# cap, but none of them are 'significant'
#UNFOLD


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
