#
# last quiz; options;
require(fOptions)
mu <- 0.06
sigma <- 0.20
Time <- 1.0
r <- 0.01
X <- 100.00
S <- 80.0

C.price <- GBSOption('c',S=S,X=X,Time=Time,r=r,b=0,sigma=sigma)
C.price

mybs <- function(S,K,sigma,Time,r=0){
  lnsk <- log(S/K)
  deno <- sigma * sqrt(Time)
  d1 <- (lnsk + (r + (sigma^2)/2) * Time) / deno
  d2 <- d1 - sigma * sqrt(Time)
  Call.P <- pnorm(d1) * S - pnorm(d2) * K * exp(-r * Time)
}
mybs.put <- function(S,K,sigma,Time,r=0) {
  Call.P <- mybs(S,K,sigma,Time,r)
  # put/call parity
  Put.P <- K * exp(-r * Time) - S + Call.P
}

C.price <- mybs(S=S,K=X,Time=Time,r=r,sigma=sigma)
C.price
P.price <- mybs.put(S=100,K=80,Time=Time,r=r,sigma=sigma)
P.price
# 1.067

lnmu <- (mu - 0.5 * sigma^2) * Time
lnsg <- sigma * sqrt(Time)

prob.loss <- pnorm(log(0.80),mean=lnmu,sd=lnsg)
# 9.41 %
