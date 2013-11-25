# Week 8 Homework 2: 
#
# * Sat Nov 24 2013 09:23:02 PM Steven E. Pav <steven@cerebellumcapital.com>
#

fdisp <- function(x) {
	cat("# ",x,"\n")
}

params <- list(gamma=2,mu=0.08,sigma=0.20,
								alpha1=-0.003,alpha2=0.012,
								beta1=1,beta2=2,
								sigma1=0.10,sigma2=0.10,
								rho=-0.5) 

# Question 1#FOLDUP
# WRONG:
#fq1 <- function(myp=params) {
	#sigma12 <- sqrt(myp$sigma1*myp$sigma2) * myp$rho
	#Sigma <- matrix(c(myp$sigma,0,0,
										#0,myp$sigma1,sigma12,
										#0,sigma12,myp$sigma2),nrow=3)
	#alps <- matrix(c(myp$mu,myp$alpha1,myp$alpha2))
	#wmarko <- solve(Sigma,alps)
	#retv <- (1/myp$gamma) * wmarko
#}
#fdisp(fq1())
##  0.2 0.02 0.07 
fq1 <- function(myp=params) {
	sigma12 <- myp$sigma1*myp$sigma2 * myp$rho
	Sigma <- matrix(c(myp$sigma^2,0,0,
										0,myp$sigma1^2,sigma12,
										0,sigma12,myp$sigma2^2),nrow=3)
	alps <- matrix(c(myp$mu,myp$alpha1,myp$alpha2))
	wmarko <- solve(Sigma,alps)
	retv <- (1/myp$gamma) * wmarko
}
fdisp(fq1())
#  1 0.2 0.7 
#UNFOLD

# Question 2#FOLDUP
fq2 <- function(myp=params) {
	transo <- matrix(c(1,0,0,-myp$beta1,1,0,-myp$beta2,0,1),nrow=3)
	retv <- transo %*% fq1(myp)
}
fdisp(fq2())
#  -0.6 0.2 0.7 
#UNFOLD

# Question 3#FOLDUP
fq3 <- function(myp=params) {
	retv <- fq2(myp)
	retv <- c(1-sum(retv),retv)
}
fdisp(fq3())
#  -0.3 -0.6 0.2 0.7 
#UNFOLD

# Question 4
# 
# correct: The negative correlation between the two manager's errors \u03b5 accounts for this result. 


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
