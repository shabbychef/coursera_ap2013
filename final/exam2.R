# Final
#
# * Wed Dec 04 2013 06:39:48 PM Steven E. Pav <steven@cerebellumcapital.com>
#

fdisp <- function(x) {
	cat("# ",x,"\n")
}

# Question 1 parts a-e#FOLDUP

# Question 1
#
# should be exp(-rho*t) * u'(c)
# so

# e^(-rho*t) * (c-h)^(-g)

# Question 2
#
# since r^f dt = - E[dLambda / Lambda]
# we have
#
# rho + g * mu / S - 1/2*g*(g+1)*(sigma/S)^2
# but S = (c - h) / c
#
#
# so 
# rho + (g * mu * c / (c-h)) - 1/2*g*(g+1)*(sigma * c / (c-h))^2


# Question 3
params = list(rho=0.05,gamma=2,c=10,h=5,mu=0.01,sigma=0.05)
fq3 <- function(myp=params) {
	S <- (myp$c - myp$h) / myp$c
	retv <- myp$rho + myp$gamma * myp$mu * S 
	retv <- retv - 0.5 * myp$gamma * (myp$gamma + 1) * (myp$sigma * S)^2
	return(retv)
}
fdisp(fq3())
#  0.05813 


# Question 4
#
# following week2 HW, this is
# g * sigma / S
# but S = (c-h)/c
# so
#
# g * sigma * c / (c-h)

# Question 5
params = list(rho=0.05,gamma=2,c=10,h=5,mu=0.01,sigma=0.05)
fq5 <- function(myp=params) {
	S <- (myp$c - myp$h) / myp$c
	retv <- myp$sigma * myp$gamma / S
	return(retv)
}
fdisp(fq5())
# 0.2
#UNFOLD

# Question 2 parts a-d#FOLDUP
#
# recall that p = E[beta * (u'(ct+1)/u'(ct)) * xt+1]
#
# the bond has payout 1 = xt+1
# 
# assuming beta = 1, u(c) = log(c), u'(c) = 1/c so
#
# p = E[(ct/(ct+1)) * 1] = E[ct/(ct+1)]


params = list(c0=1.0,cofu=1.5,cofd=0.75,pofu=0.5,pofd=0.5)

fq2gen <- function(xup,xdn,myp=params) {
	retv <- xup * myp$pofu * (myp$c0 / myp$cofu) + xdn * myp$pofd * (myp$c0 / myp$cofd)
	return(retv)
}


# Question 6

fq6 <- function(myp=params) {
	retv <- fq2gen(1,1,myp)
	return(retv)
}
fdisp(fq6())
# 1

# Question 7
# just use
# p = E[(ct/(ct+1)) * xt] 
fq7 <- function(myp=params) {
	priceA <- fq2gen(1,-1,myp)
	priceB <- fq2gen(-1,1,myp)
	retv <- c(priceA,priceB)
	return(retv)
}
fdisp(fq7())
#  -0.3333 0.3333 

# Question 8
fq8 <- function(myp=params) {
	priceA <- fq2gen(1,0,myp)
	priceB <- fq2gen(0,1,myp)
	retv <- c(priceA,priceB)
	return(retv)
}
fdisp(fq8())
#  0.3333 0.6667 

# hmm. find m
fq9 <- function(myp=params) {
	rf <- fq2gen(1,1,myp)
	priceU <- fq2gen(1,0,myp)
	priceD <- fq2gen(0,1,myp)
	retv <- rf * c(priceU,priceD)
	return(retv)
}
fdisp(fq9())
#  0.3333 0.6667 
#UNFOLD

# Question 3 parts a-d#FOLDUP
#
# see also Homework: Conditional and Unconditional Frontiers 

params = list(pof1=0.5,pof2=0.5,mu1=0.08,mu2=0.08,
							sg1=0.16,sg2=0.24)

q3help <- function(myp=params) {
	# compute the conditional uncentered second moments.
	sm1 <- params$mu1^2 + params$sg1^2
	sm2 <- params$mu2^2 + params$sg2^2
	retv <- c(myp,sm1=sm1,sm2=sm2)
	return(retv)
}

fq10 <- function(myp=params) {
	myp <- q3help(myp)
	mom1 <- myp$pof1 * myp$mu1 + myp$pof2 * myp$mu2
	mom2 <- myp$pof1 * myp$sm1 + myp$pof2 * myp$sm2
	uvar <- mom2 - mom1^2
	usr <- mom1 / sqrt(uvar)
	return(usr)
}
fdisp(fq10())
#  0.3922 

# Question 11
#
# this is Q 2 from Homework Conditional and Unconditional Frontiers
# which I got all wrong.
# fuckity. but the help says that
# w = E[Re] / E[Re^2] works
#
# beware second moment vs. covariance!
fq11 <- function(myp=params) {
	myp <- q3help(myp)
	wof1 <- myp$mu1 / myp$sm1
	wof2 <- myp$mu1 / myp$sm2
	retv <- c(wof1,wof2)
	# "Check that your Re∗ has the right properties: Et(Re∗Re)=Et(Re)"
	Ere <- myp$pof1 * myp$mu1 + myp$pof2 * myp$mu2
	rhs <- wof1 * myp$pof1 * myp$sm1 + wof2 * myp$pof2 * myp$sm2
	cat(sprintf("error is %.3g\n",Ere - rhs))
	

	return(retv)
}
fdisp(fq11())
#  2.5 1.25 

# Question 12
# this is just Q4 off the old HW
#
# I dunno how I got this, regrettably:
# 0.416666

# Question 13
# also lost to the sands of time; this is Q7 off the old HW
# 1.3333 0.66666

#UNFOLD

# Question 5 parts a-c#FOLDUP

irt2 <- 1 / sqrt(2)
irt3 <- 1 / sqrt(3)
irt6 <- 1 / sqrt(6)
Q <- matrix(c(irt3,irt3,irt3,  -irt2,0,irt2,   -irt6,2*irt6,-irt6),nrow=3)
Lam <- diag(c(3.00,1.00,0.06))
Sig <- Q %*% Lam %*% t(Q)

# Question 18
# I believe the answer is just an eigenvector; the maximal one at that.
# trick question?

v <- Q[,1]
(t(v) %*% Sig %*% v) / (t(v) %*% v)
fdisp(v)
#  0.5774 0.5774 0.5774 

# Question 19

require(mvtnorm)

set.seed(1732491)
X <- rmvnorm(1e8, sigma=Sig)
f1 <- X %*% v
# now just do the regression to confirm;
foomod <- lm(X ~ f1)
fdisp(foomod$coefficients["f1",])

fdisp(v)
#  0.5774 0.5774 0.5774 

# Question 20
X1 <- X[,1]
foomod2 <- lm(X1 ~ f1)
sfoo <- summary(foomod2)
fdisp(sfoo$r.squared)
#  0.6624 

# shouldn't it be one of these? harumph
L1 <- diag(Lam)
L2 <- L1 ^ 2
Lh <- sqrt(L1)
fdisp(L2[1] / sum(L2))
fdisp(L1[1] / sum(L1))
fdisp(Lh[1] / sum(Lh))


#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
