# "meet the data"
#
#

pry <- read.delim("ps1_data.txt",sep="",
									header=FALSE,comment.char='%',
									skip=5,
									strip.white=TRUE)

colnames(pry) <- c("Date","Return","DbyP","Dgrowth","Tbill")
pry$excess.Ret <- pry$Return - pry$Tbill 

TEO <- as.Date(as.character(pry$Date),format="%Y%m%d") 


qprint <- function(x) {
	cat("# ",x,"\n")
}

# question 1
q1 <- c(100 * mean(pry$Return),
				100 * mean(pry$Tbill),
				100 * mean(pry$excess.Ret),
				100 * sd(pry$Return),
				100 * sd(pry$Tbill),
				100 * sd(pry$excess.Ret))
qprint(q1)

# question 2, SR
sr <- mean(pry$excess.Ret) / sd(pry$excess.Ret)
qprint(sr)
				
# question 3, plots, acf
acf(pry$Return)
acf(pry$Tbill)
acf(pry$DbyP)
# conclusion: Return uncorrelated; Tbill and DbyP correlated.

lmsum <- function(foomod) {
	summod <- summary(foomod)
	retv <- c(summod$coefficients[2,1],
						summod$coefficients[2,3],
						summod$r.squared)
	return(retv)
}

lmsum2 <- function(regx,regy) {
	foomod <- lm(regy ~ regx)
	retv <- lmsum(foomod)
	summod <- summary(foomod)
	retv <- c(retv,
						100 * mean(regy),
						100 * summod$coefficients[2,1] * sd(regx))
	return(retv)
}

# question 4
# autocorrelationish function
freg <- function(x) {
	regy <- x[2:length(x)]
	regx <- x[1:(length(x)-1)]
	retv <- lmsum2(regx,regy)
	return(retv)
}

# just a looksee:
print(round(freg(pry$Return),2))
#print(round(freg(pry$Tbill),2))
#print(round(freg(pry$excess.Ret),2))

qprint(round(c(freg(pry$Return),
							freg(pry$Tbill),
							freg(pry$excess.Ret)),2))

#  0.01 0.06 0 11.62 0.13 0.92 20.42 0.83 3.95 3.16 0.01 0.11 0 7.66 0.25 

# question 5
regy <- pry$excess.Ret[2:dim(pry)[1]]
regx <- pry$DbyP[1:(dim(pry)[1]-1)]
foomod <- lm(regy ~ regx)
qprint(round(lmsum2(regx,regy),2))
#  3.68 2.54 0.07 7.66 5.54 


# question 6
qprint(summary(foomod))
# yes, yes

# question 7

# simple boxcarsum
sboxsum <- function(x,win=5) {
	filcoef <- rep(1,win)
	sx <- filter(x,filcoef,method="convolution",sides=1)
	return(sx)
}
# simple boxcar product;
sboxprod <- function(x,...) {
	sx <- exp(sboxsum(log(x),...))
	return(sx)
}
# simple boxcar returns
sboxret <- function(x,...) {
	sx <- sboxprod(1 + x,...) - 1
	return(sx)
}

wins <- 5
bret.stock <- sboxret(pry$Return,win=wins)
bret.bond <- sboxret(pry$Tbill,win=wins)
bret.exc <- bret.stock - bret.bond

regy <- bret.exc[(wins+1):length(bret.exc)]
regx <- pry$DbyP[1:(dim(pry)[1] - wins)]

foomod <- lm(regx ~ regy)
lmres <- round(lmsum(foomod),2)
qprint(lmres)

# apparently the 0.01 is wrong. bad annualization?
# 0.01 4.99 0.24
foomod.overlap <- foomod

# question 8
#  cause and effect OK
#  rut-roh
#  uncorrelated errors?
#  maybe my variables are off...

# not uncorrelated:
plot(regx,foomod$residuals)

#  never normally distributed, but not bad:
qqnorm(foomod$residuals)

#  heteroskedastic!
plot(foomod$residuals)

# correlated errors.
acf(foomod$residuals)


# question 9
# everything but inconsistency?
#
# apparently:
#
# too small se, t-stat inflated.
# b estimator inefficient.

# question 10
yrow <- seq(from=(wins+1),to=length(bret.exc),by=wins)
xrow <- yrow - wins

regy <- bret.exc[yrow]
regx <- pry$DbyP[xrow]

foomod <- lm(regx ~ regy)
lmres <- round(lmsum(foomod),2)
qprint(lmres)
#  0.01 2.52 0.3 
#  once again, the 0.01 is wrong. bummer.

# question 11
# only the t-stats really changed. cf. Valkanov.

# question 12
predicted <- foomod$fitted.values
ploty <- cbind(regy,predicted) 

# fuck this
plot(t(cbind(TEO[yrow],TEO[yrow])),
		 t(matrix(ploty,ncol=2)),col=c('red','blue'))

# fuck that. as xts?

require(xts)

rxts <- xts(ploty,order.by=TEO[yrow])
plot(rxts)

# FUCK.
plot(rxts$regy)
par(new=TRUE)
plot(rxts$predicted,axes=FALSE)  # that the trick?

# d/p varies less;
#
# apparently: dividend yield forecasts 5-10 year swings early,
# but seems to slow down.
# I don't see this by eye, but whatever.
# 


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
