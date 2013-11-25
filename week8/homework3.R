# Week 8 Homework 2: 
#
# * Sat Nov 24 2013 10:02:47 PM Steven E. Pav <steven@cerebellumcapital.com>
#

fdisp <- function(x) {
	cat("# ",x,"\n")
}

params <- list(gamma=2,mu=0.08,sigma=0.20,b=4,EDP=0.04)

# Question 1#FOLDUP
fq.se <- function(per.T,myp=params) {
	thse <- myp$sigma / sqrt(per.T)
}
fq1 <- function(myp=params) {
	sapply(c(5,20,50),fq.se)
}
fdisp(100 * fq1())
#  8.944 4.472 2.828 
#UNFOLD

# Question 2#FOLDUP
fq2 <- function(per.T,myp=params) {
	se <- fq.se(per.T,myp=myp)
	w <- myp$mu / (myp$gamma * (se^2 + myp$sigma^2))
}
resu <- 100 * sapply(c(5,20,50,Inf),fq2)
fdisp(resu)
#  83.33 95.24 98.04 100 
#UNFOLD

# Question 3#FOLDUP
fq3 <- function(h,per.T=20,myp=params) {
	se <- h * fq.se(per.T,myp=myp)
	w <- (h * myp$mu) / (myp$gamma * (se^2 + h * myp$sigma^2))
}
resu <- 100 * sapply(c(1,5,20),fq3)
fdisp(resu)
#  95.24 80 50 
#UNFOLD

# Question 4#FOLDUP
fq4 <- function(DP,myp=params) {
	w <- (1/myp$gamma) * (myp$mu + myp$b * (DP - myp$EDP)) / (myp$sigma^2)
}
xv <- seq(0,0.15,length.out=101)
yv <- 100 * sapply(xv,fq4)
plot(xv,yv)
resu <- 100 * sapply(c(2,4,6) / 100,fq4)
fdisp(resu)
#  0 100 200 
#UNFOLD

# Question 5#FOLDUP
#fq5 <- function(DP,sigma.b=2,myp=params) {
	#w <- (1/myp$gamma) * (myp$mu + myp$b * (DP - myp$EDP)) / ((sigma.b)^2 + myp$sigma^2)
#}
#resu <- 100 * sapply(c(2,4,6) / 100,fq5)
#fdisp(resu)
##  0 0.9901 1.98 
##  well, the 0 is correct;

fq5 <- function(DP,sigma.b=2,myp=params) {
	del.DP <- DP - myp$EDP
	Eret <- myp$b * del.DP
	Sret <- myp$sigma^2 + (sigma.b^2) * del.DP^2
	w <- (1/myp$gamma) * (myp$mu + Eret) / (Sret)
}
resu <- 100 * sapply(c(2,4,6) / 100,fq5)
fdisp(resu)
#  0 100 192.3 


#UNFOLD

# Question 6#FOLDUP
xv <- seq(0,0.15,length.out=101)
yv <- 100 * sapply(xv,fq5)
yv.old <- 100 * sapply(xv,fq4)
plot(xv,yv)
lines(xv,yv.old)

# WRONG: The weights with parameter uncertainty have a distinctly lower slope
# as a function of DP, indicating a lot less exploitation of predictability.
# CORRECT: Near the mean DP=0.04, the lines are about the same. Paramter uncertainty makes portfolio weights a nonlinear function which has the biggest effects in sharply reducing the portoflio weight for large DP relative to the standard advice. 


#UNFOLD


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
