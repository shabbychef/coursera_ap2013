# Week 7 Homework 3: 
#
# * Sat Nov 23 2013 09:37:08 PM Steven E. Pav <steven@cerebellumcapital.com>
#


fdisp <- function(x) {
	cat("# ",x,"\n")
}

sig2 <- 0.1
phi <- 0.9
lam <- 0.5

# Question 1#FOLDUP
fq1 <- function(phi=0.9,sig2=0.1,lam=0.5) {
	a2 <- 0.5 * sig2
	b2 <- - (1+phi) + sig2 * lam
	c(a2,b2)
}
fdisp(fq1())
#  0.05 -1.85 
#UNFOLD
# Question 2#FOLDUP
fq2 <- function(...) {
	sub.resu <- fq1(...)
	c2 <- - sub.resu[1]
	d2 <- - (1 + sub.resu[2])
	c(c2,d2)
}
fdisp(fq2())
#  -0.05 0.85 
#UNFOLD
# Question 3#FOLDUP
fq3 <- function(phi=0.9,sig2=0.1,lam=0.5) {
	sub.resu <- fq1(phi=phi,sig2=sig2,lam=lam)
	q3a <- - sub.resu[1]
	q3b <- - (1 + phi + sub.resu[2])
	c(q3a,q3b)
}
fdisp(fq3())
#  -0.05 -0.05 
#UNFOLD
# Question 4#FOLDUP
fq4 <- function(phi=0.9,sig2=0.1,lam=0.5) {
	resu2 <- fq2(phi=phi,sig2=sig2,lam=lam)
	resu3 <- fq3(phi=phi,sig2=sig2,lam=lam)
	slope1 <- resu3[2] / (resu2[2] - 1)
}

fdisp(c(fq4(),fq4(phi=1)))
#  0.3333 1 
#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
