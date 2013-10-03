# Week 1 Homework 1: A Little Model of Time-Varying Expected Returns 
#
# * Thu Oct 03 2013 09:09:27 AM Steven E. Pav <steven@cerebellumcapital.com>
#
# σε=0.018, ϕ=0.94, σδ=0.18 and the correlation between εt and δt shocks ρ=−ϕ1−ϕ2σεσδ=−0.80756
#

sig.epsi <- 0.018
phi <- 0.94
sig.dlta <- 0.18
rho <- - (phi / (1 - phi^2)) * (sig.epsi / sig.dlta)

fdisp <- function(x) {
	cat("# ",x,"\n")
}

# Question 1

bhat <- 1  # I think it is unbiased;
sigx <- (1 / (1 - phi)) * sig.epsi
varret <- sigx^2 + sig.dlta^2

resu <- c(bhat,bhat * sigx,sqrt(varret),
					(bhat * sigx / sqrt(varret))^2)
fdisp(resu)
#  1 0.3 0.3499 0.7353 

# Question 2
# simulate an MA sequence

require(MASS)

nsim <- 1e6

fsym <- function(a11,a12,a22) {
	matrix(c(a11,a12,a12,a22),nrow=2)
}
Sigma <- fsym(sig.epsi^2,sig.dlta*sig.epsi*rho,sig.dlta^2)

set.seed(as.integer(charToRaw("df40ec01-f1c4-4f59-babc-1423ce942623")))
epsdl <- mvrnorm(n=nsim,mu=c(0,0),Sigma=Sigma)
epsi <- epsdl[,1]
dlta <- c(epsdl[2:dim(epsdl)[1],2],0)

xobs <- filter(x=epsi,filter=c(phi),method="recursive",sides=1)
robs <- xobs + dlta

foolm <- lm(robs ~ xobs)
#print(summary(foolm))

if (nsim < 2e4)
	plot(xobs,robs)

# now forward filter the r?
r5 <- filter(robs,filter=rep(1,5),method="conv",sides=1)
r5 <- r5[5:length(r5)]
x5 <- xobs[1:length(r5)]
foolm5 <- lm(r5 ~ x5)
sumfoo <- summary(foolm5)
print(sumfoo)

resu <- c(foolm5$coefficients[2],sumfoo$r.squared)
fdisp(resu)
#  4.419 0.3099 

# Question 3
ntake <- 30

prdr1 <- foolm$fitted.values[1:ntake]
actr1 <- robs[1:ntake]

prdr5 <- foolm5$fitted.values[1:ntake]
actr5 <- r5[1:ntake]

plot(rep(1:ntake,2),t(rbind(prdr1,actr1)),col=c("red","blue"))

plot(rep(1:ntake,2),t(rbind(prdr5,actr5)),col=c("red","blue"))
sd(prdr5)
sd(actr5)

# I think it is
#
# one year return is more jumpy -- less serially correlated
# 







#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
