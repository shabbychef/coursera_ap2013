# Week 4 Homework 1: Mean Variance Frontier
#
# * Fri Oct 25 2013 09:41:56 AM Steven E. Pav <steven@cerebellumcapital.com>

fcat <- function(x) {
	cat("# ",x,"\n")
}

# Question 1#FOLDUP
#
# Hints: When R denotes a vector of returns, I defined in lecture
#R∗=1′E(RR′)−1R / 1′E(RR′)−11
#You can approach this problem by implementing that formula. The vector of returns is [RfR]′. The properties we're checking derive from E(x∗R)=1 for any return. Now, from the definition of R∗ from x∗ you should be able to figure out what E(R∗R) and E(R∗Re) should be for any R and Re. R∗ will be quite short -- a negative weight on R, so it ends up on the lower part of the frontier. 

rf <- 0.05
r1 <- 0.10
sigma <- 0.20
muvec  <- c(rf,r1)
dim(muvec) <- c(2,1)
Sigma  <- matrix(c(0,0,0,sigma^2),nrow=2)  
Errprime <- Sigma + muvec %*% t(muvec)
Einv <- solve(Errprime)
Wpass <- colSums(Einv) / sum(Einv) 
fcat(Wpass[,2])

# same same.
LeftE <- t(solve(Errprime,rep(1,2)))
Wpass <- LeftE / sum(LeftE)

fcat(Wpass[,2])
# WRONG
#  -0.05882 
#UNFOLD

# Question 2#FOLDUP
# 
#Hint: I gave the formula in lecture,
#Re∗=E(Re′)E(ReRe′)−1Re
#The property you want to check is that Re∗ generates means in excess returns the same way x∗ generates prices among payoffs. 

ERe <- r1 - rf
SigRe <- sigma^2 + ERe^2
wt <- ERe / SigRe
fcat(wt)
# RIGHT!
#  1.176 

#UNFOLD

# load data#FOLDUP
really.read.delim <- function(url,...) {
	temporaryFile <- tempfile()
	download.file(url,destfile=temporaryFile, method="curl")
	retval <- read.table(temporaryFile,...)
}

ff3 <- really.read.delim("https://d396qusza40orc.cloudfront.net/assetpricing%2Fdata%2Fff_factors.txt",
											 skip=7,
											 check.names=FALSE,
											 col.names=c("date","Mkt.RF","SMB","HML","RF"))
head(ff3)

ff25 <- really.read.delim("https://d396qusza40orc.cloudfront.net/assetpricing%2Fdata%2Fff_25.txt",
													comment.char='%',
													check.names=FALSE)
colnames(ff25) <- c("date",sapply(1:25,function(n) { sprintf("port_%02d",n) })) #UNFOLD

# Question 3:#FOLDUP
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
#
# [RIGHT?] The weights have strong long (positive) and short (negative) values, spread pretty much randomly across the size and book/market portfolios
#UNFOLD

# not sure. WTF.
# make R*
Rs <- sub25[,2:26]
Sigma <- cov(Rs)
mu <- colMeans(Rs)
SM <- Sigma + mu %*% t(mu)
Wstar <- solve(SM,rep(1,length(mu)))
Rstar <- as.matrix(Rs) %*% Wstar

# Question 6#FOLDUP
Sigma <- cov(Re)
allone <- rep(1,dim(Sigma)[1])
Wgmv <- solve(Sigma,allone)
Wgmv <- Wgmv / sum(Wgmv)  # normalize?
Remv <- as.matrix(Re) %*% Wgmv

mak.frontier <- function(rs,roth,w=seq(-100,100,length.out=500)) {
	rboth <- cbind(rs,roth)
	mu <- colMeans(rboth)
	Sg <- cov(rboth)
	yval <- mu[1] * w + mu[2] * (1 - w)
	xval <- sqrt(Sg[1,1] * w^2 + 2 * Sg[1,2] * w * (1-w) + Sg[2,2] * (1-w)^2)
	retval <- list(xval=xval,yval=yval)
}
front.points <- function(rs,...) {
	text(sd(rs),mean(rs),labels=deparse(substitute(rs)))
	points(sd(rs),mean(rs),...)
}

fone <- mak.frontier(Rstar,Restar,w=seq(-20,20,length.out=500))
ftwo <- mak.frontier(Rstar,Remv,w=seq(-50,50,length.out=500))

plot(fone$xval,fone$yval,col='red',type='l',
		 xlab='vol',ylab='return')
lines(ftwo$xval,ftwo$yval,col='blue')
front.points(Remv,col='magenta',type='p')
front.points(Restar,col='blue',type='p',pch=24)
front.points(subf$SMB,col='red',type='p',pch=21)
front.points(subf$HML,col='green',type='p',pch=22)
front.points(subf$Mkt.RF,col='black',type='p',pch=23)

# I picked:
# [WRONG] The Remv frontier is larger than the Re∗ frontier, as Remv explicitly minimizes variance for given mean in sample. 
# [WRONG] The Remv frontier is exactly the same as the Re∗ frontier. These are two ways of calculating the same object. Remv is on the top half of the frontier. Re∗, like the minimum second moment return R∗, is on the bottom half of the frontier.
#UNFOLD

# Question 7#FOLDUP
# something wrong with my plot
# totally fucked.
# I picked
# [WRONG] Remv lies a bit further out than Re∗. Both portfolios lie near the FF factor portfolios, as the latter are efficient ways of capturing the information in the 25 
# [WRONG] Remv has exactly the same mean and variance as Re∗. These are two ways of calculating the same thing. Both portfolios lie near the FF factor portfolios, as the latter are efficient ways of capturing the information in the 25.
#UNFOLD

# Question 8#FOLDUP
#
for (iii in 1:dim(Re)[2]) {
	front.points(Re[,iii],col='cyan',type='p',pch=23)
}
# fuck. I picked
# [RIGHT] The factors rmrf hml and smb, and the original 25 assets all seem to lie well inside the frontier. 
##UNFOLD

# Question 9#FOLDUP
y <- as.matrix(Re)
foolm <- lm(y ~ Restar)
betas <- foolm$coefficients[2,] 
mrets <- colMeans(y)
plot(betas,mrets)

blah <- lm(mrets ~ betas)

# [RIGHT] The 25 mean return - beta pairs lie exactly on a straight line going through the origin, verifying the Roll theorem. 
#UNFOLD

# Question 10#FOLDUP
Etwo <- colMeans(Re * Restar)
Eone <- colMeans(Re)
fcat(c(Etwo[1],Eone[1]))
#  0.3163 0.3166 
#UNFOLD

# I SWAG'ed 11 through 13, guessing:
#
# 11
# [WRONG] The out-of-sample frontier falls apart completely. It lies below the market Sharpe ratio (line from origin to rmrf.) 
# [RIGHT] The out-of-sample frontier falls to about the same value as the frontier generated by the portfolios, still about twice the market Sharpe ratio and just above the individual 25 portfolios. 
#
# 12
# [RIGHT] The frontier formed from the three factor portfolios is not much bigger in sample than out of sample. 
# 13
# [RIGHT] The relationship between mean excess return and beta out of sample is destroyed, the points have a big scatter and only a very slight positive slope is detectable. 


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
