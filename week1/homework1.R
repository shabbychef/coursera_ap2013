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

# Question 1#FOLDUP

bhat <- 1  # I think it is unbiased;
sigx <- (1 / (1 - phi)) * sig.epsi
varret <- sigx^2 + sig.dlta^2

resu <- c(bhat,bhat * sigx,sqrt(varret),
					(bhat * sigx / sqrt(varret))^2)
fdisp(resu)
#  the 1 is correct, but the others, not so much.
#  1 0.3 0.3499 0.7353 
#
# try again:
sigx <- sig.epsi
varret <- sig.dlta^2
resu <- c(bhat,bhat * sigx,sqrt(varret),
					(bhat * sigx / sqrt(varret))^2)
fdisp(resu)
#  1 0.018 0.18 0.01 
#
# also wrong.
# duh, the problem is you cannot express the AR
# as a MA b/c this misrepresents the series. 
# so, the right answer comes from
#
# sigma^2 = phi^2 sigma^2 + sigma_eps^2
# sigma^2 (1-phi^2) = sigma_eps^2
# and proceed.
sigx <- sig.epsi / sqrt(1-phi^2)
varret <- sigx^2 + sig.dlta^2
resu <- c(bhat,bhat * sigx,sqrt(varret),
					(bhat * sigx / sqrt(varret))^2)
fdisp(resu)
#  1 0.05276 0.1876 0.07911 
#
# crap. shoulda paid attention to my sims.

#
#  #UNFOLD

# Question 2#FOLDUP
# simulate an MA sequence

require(MASS)

nsim <- 5e6

fsym <- function(a11,a12,a22) {
	matrix(c(a11,a12,a12,a22),nrow=2)
}
Sigma <- fsym(sig.epsi^2,sig.dlta*sig.epsi*rho,sig.dlta^2)

set.seed(as.integer(charToRaw("df40ec01-f1c4-4f59-babc-1423ce942623")))
epsdl <- mvrnorm(n=nsim,mu=c(0,0),Sigma=Sigma)
epsi <- epsdl[,1]
dlta <- epsdl[,2]
xobs <- filter(x=epsi,filter=c(phi),method="recursive",sides=1)
robs <- c(NA,xobs[1:(length(xobs)-1)] + dlta[2:length(dlta)])

# now realize that we are suppose to regress
# r_{t+1} against x_t
xobs <- as.vector(xobs[1:(length(xobs)-1)])
robs <- as.vector(robs[2:length(robs)])

foolm <- lm(robs ~ xobs)
sumfoo <- summary(foolm)
print(sumfoo)

if (nsim < 2e4)
	plot(xobs,robs)

resu <- c(foolm$coefficients[2],sumfoo$r.squared)
fdisp(resu)
#  1 0.07925 

resu <- c(foolm$coefficients[2],sd(foolm$fitted.values),
					sd(robs),sumfoo$r.squared)
fdisp(resu)
#  1 0.05281 0.1876 0.07925 
fdisp((sd(foolm$fitted.values)/sd(robs))^2)

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
# these were deemed to be correct
#UNFOLD

# Question 3#FOLDUP
ntake <- 30

prdr1 <- foolm$fitted.values[1:ntake]
actr1 <- robs[1:ntake]

prdr5 <- foolm5$fitted.values[1:ntake]
actr5 <- r5[1:ntake]

plot(rep(1:ntake,2),t(rbind(prdr1,actr1)),col=c("red","blue"))

plot(rep(1:ntake,2),t(rbind(prdr5,actr5)),col=c("red","blue"))
#sd(prdr5)
#sd(actr5)

# I think it is
#
# checked: 
#  one year return is more jumpy -- less serially correlated
# 
# unchecked:
#  expected return lines more correlated with actual in 1 yr than 5 yr
#  1 yr return plot shows larger returns than the 5 yr
#  expected return lines seem to move before the actual returns move
# 
# all correctly entered.#UNFOLD

# Question 4#FOLDUP
#vt <- foolm$residuals
#vnext <- vt[2:length(vt)]
rprev <- robs[1:(length(robs)-1)]
rnext <- robs[2:length(robs)]

foolm2 <- lm(rnext ~ rprev)
sumfoo <- summary(foolm2)

resu <- c(foolm2$coefficients[2],sumfoo$r.squared)
fdisp(resu)
#  -0.0002795 7.809e-08 
#  I entered 0 0
#  both correct#UNFOLD

# question 5. #FOLDUP
# WTF?
# I answered none.
# evidently you want
# check: Even with this predictability model, unconditional variacne
# is still linear in horizon k, so stocks are no safer in the long run
# uncheck: the predictability of returns means that long horizon ...
# face less risk than short.
##UNFOLD


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
