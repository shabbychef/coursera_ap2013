# Week 7 Homework 1: 
#
# * Wed Nov 20 2013 01:48:39 PM Steven E. Pav <steven@cerebellumcapital.com>
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

fbliss <- read.delim(fb.url,comment.char='%',header=FALSE)

require(xts)
TEO <- as.Date(as.character(fbliss$date),"%Y%m%d")
TEO <- as.POSIXct(TEO)
fxts <- xts(fbliss[,!(colnames(fbliss) %in% "date")],order.by=TEO)

# ok, finally. 
# divide by 100, then take the log
log.p <- log((1/100) * fxts)

# compute the yields
yield <- -log.p / col(fxts)

# the forward prices
forward <- t(- apply(t(log.p),2,diff))

# Question 1#FOLDUP
# average log yield
ans <- 100 * colMeans(yield) 
fdisp(ans)
#  5.08 5.278 5.456 5.601 5.704 

# OOPS. start at observation 140; duh;
sub.xts <- fxts["1964-01-30::2013-06-27",]
log.p <- log((1/100) * sub.xts)

# compute the yields
yield <- -log.p / col(sub.xts)

# average log yield
ans <- 100 * colMeans(yield) 
fdisp(ans)
#  5.631 5.842 6.022 6.177 6.285 

#UNFOLD

# Question 2#FOLDUP
# annual log returns
sub.xts <- fxts["1964-01-30::2013-06-27",]
head(sub.xts)
tail(sub.xts)

sub.log.p <- log((1/100) * sub.xts)
rets <- sub.log.p[13:nrow(sub.log.p),1:(ncol(sub.log.p)-1)] - as.matrix(sub.log.p[1:(nrow(sub.log.p)-12),2:(ncol(sub.log.p))])

# excess returns
sub.yield <- - sub.log.p[1:(nrow(sub.log.p)-12),1]
rets.x <- rets - rep(as.matrix(sub.yield),ncol(rets))

require(SharpeR)

as.sr(rets.x)
# oik

mu <- apply(rets.x,2,mean)
sg <- apply(rets.x,2,sd)
sr <- mu / sg
fdisp(sr)
#UNFOLD

# Question 3#FOLDUP
# the forward prices
sub.forward <- t(- apply(t(sub.log.p),2,diff))

reg.y <- rets.x
reg.x <- sub.forward - rep(as.matrix(- sub.log.p[,1]),ncol(sub.forward))
reg.x <- reg.x[1:(nrow(reg.x)-12),]

b.vals <- rep(NA,ncol(reg.x))

for (iii in 1:ncol(reg.x)) {
	sub.x <- reg.x[,iii]
	sub.y <- reg.y[,iii]
	foo.mod <- lm(sub.y ~ sub.x)
	b.vals[iii] <- foo.mod$coefficients[2] 
}
fdisp(b.vals)
reg1 <- b.vals
#  0.8355 1.146 1.394 1.14 
#UNFOLD

# Question 4#FOLDUP
yield.one <- - sub.log.p[,1]
b.vals <- rep(NA,ncol(reg.x))

for (iii in 1:ncol(reg.x)) {
	nshift <- 12*iii
	sub.y <- yield.one[(1+nshift):nrow(yield.one),] - as.matrix(yield.one[1:(nrow(yield.one)-nshift),])
	sub.x <- reg.x[1:length(sub.y),iii]
	foo.mod <- lm(sub.y ~ sub.x)
	b.vals[iii] <- foo.mod$coefficients[2] 
}
fdisp(b.vals)
#  0.1645 0.5035 0.7674 0.895 
reg2 <- b.vals

reg1 + reg2
#UNFOLD

# Question 5#FOLDUP
# I believe the answer is that
# z = 1, x = n-1, w = x

resu <- NULL
for (n.val in (4:5)) {
	z <- 1
	x <- n.val - 1
	w <- x
	resu <- c(resu,c(w,x,z))
}
fdisp(resu)
#  3 3 1 4 4 1 
#UNFOLD

# Question 6#FOLDUP
# I think we have x=n, z=n-1, w=n-1, v=n-1
resu <- NULL
for (n.val in (4:5)) {
	x <- n.val
	z <- n.val-1
	w <- n.val-1
	v <- n.val-1
	resu <- c(resu,c(x,z,w,v))
}
fdisp(resu)
#  4 3 3 3 5 4 4 4 
#UNFOLD

# Question 7#FOLDUP
# aiii! what?
# assume the return is the forward rate? ack.
p.t1 <- c(-0.05,-0.15,-0.30,-0.45)
f.rat <- diff(p.t1)
e.val <- p.t1[2:length(p.t1)] - f.rat
fdisp(e.val)
#  -0.05 -0.15 -0.3 
#UNFOLD


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
