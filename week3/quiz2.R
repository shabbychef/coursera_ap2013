# Week 3 Quiz: Risk-Neutral Probabilities (practice version) 
#
# * Thu Oct 17 2013 01:49:55 PM Steven E. Pav <steven@cerebellumcapital.com>

u <- 1.5
d <- 0.9
rf <- 1.1
p.u <- 2/3
p.d <- 1 - p.u

Msub <- t(matrix(c(u,1,d,1),ncol=2))
combo.u <- solve(Msub,c(1,0))
combo.d <- solve(Msub,c(0,1))

cur.price <- c(1,1/rf)

cost.u <- combo.u %*% cur.price 
cost.d <- combo.d %*% cur.price

fcat <- function(x){ cat("#",x,"\n") }

fcat(c(cost.u,cost.d))
# 0.6061 0.303 

m.u <- cost.u / p.u
m.d <- cost.d / p.d

fcat(c(m.u,m.d))
# 0.9091 0.9091 

pistar.u <- m.u * rf * p.u
pistar.d <- m.d * rf * p.d
fcat(c(pistar.u,pistar.d))
# 0.6667 0.3333 

# and again:#FOLDUP
p.u <- 1/2
p.d <- 1 - p.u

Msub <- t(matrix(c(u,1,d,1),ncol=2))
combo.u <- solve(Msub,c(1,0))
combo.d <- solve(Msub,c(0,1))

cur.price <- c(1,1/rf)

cost.u <- combo.u %*% cur.price 
cost.d <- combo.d %*% cur.price

fcat <- function(x){ cat("#",x,"\n") }

fcat(c(cost.u,cost.d))
# 0.6061 0.303 

m.u <- cost.u / p.u
m.d <- cost.d / p.d

fcat(c(m.u,m.d))
# 1.212 0.6061 

pistar.u <- m.u * rf * p.u
pistar.d <- m.d * rf * p.d
fcat(c(pistar.u,pistar.d))
# 0.6667 0.3333 

#UNFOLD

# my answers were all wrong.
# q4 was right:
# correcto: The discount factor changes, the contingent claims prices and risk neutral probabilities are unaffected.
# q5 was wrong:
# this is incorrect: Risk-neutral probabilities and contingent claims prices change, stock and bond prices (1) do not change
# this is correcto:
# Stock and bond prices change, risk-neutral probabilities change, contingent claims prices change.

fcat <- function(x){ cat("#",x,"\n") }

# try with real numbers#FOLDUP
u <- 1.5
d <- 0.9
rf <- 1.1
p.u <- 2/3
p.d <- 1 - p.u

Msub <- t(matrix(c(u,1,d,1),ncol=2))
combo.u <- solve(Msub,c(1,0))
combo.d <- solve(Msub,c(0,1))

cur.price <- c(1,1/rf)

cost.u <- combo.u %*% cur.price 
cost.d <- combo.d %*% cur.price

fcat(c(cost.u,cost.d))
# 0.303 0.6061 

m.u <- cost.u / p.u
m.d <- cost.d / p.d

fcat(c(m.u,m.d))
# 0.4545 1.818 

pistar.u <- m.u * rf * p.u
pistar.d <- m.d * rf * p.d
fcat(c(pistar.u,pistar.d))
# 0.3333 0.6667 

#UNFOLD

# try with diff numbers#FOLDUP
u <- 1.5
d <- 0.9
rf <- 1.1
p.u <- 0.1
p.d <- 1 - p.u

Msub <- t(matrix(c(u,1,d,1),ncol=2))
combo.u <- solve(Msub,c(1,0))
combo.d <- solve(Msub,c(0,1))

cur.price <- c(1,1/rf)

cost.u <- combo.u %*% cur.price 
cost.d <- combo.d %*% cur.price

fcat(c(cost.u,cost.d))
# 0.303 0.6061 

m.u <- cost.u / p.u
m.d <- cost.d / p.d

fcat(c(m.u,m.d))
# 0.4545 1.818 

pistar.u <- m.u * rf * p.u
pistar.d <- m.d * rf * p.d
fcat(c(pistar.u,pistar.d))
# 0.3333 0.6667 

#UNFOLD


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
