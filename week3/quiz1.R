# Week 3 Quiz: States and Complete Markets (practice version)
#
# * Thu Oct 17 2013 01:24:02 PM Steven E. Pav <steven@cerebellumcapital.com>
#


u <- 1.2
d <- 0.9

Msub <- t(matrix(c(u,1,d,1),ncol=2))
v1 <- solve(Msub,c(1,0))
v2 <- solve(Msub,c(u,1))
v2 <- solve(Msub,c(u,0))

M <- matrix(c(u,1,u,1,0,0,  u,1,d,1,0,0,  d,1,0,0,u,1,  d,1,0,0,d,1),ncol=4)
M <- t(M)

prop <- c(10/6,-3/2,10/3,-7/2,0,0)
M %*% prop
print(prop)

10/6 - 3/2

# ok, that was wrong. try again.
# 4.0 -3.6 3.333 -3 0 0 0.4
# fuck. wrong.


# Q2:
# 2 -2 -1 0 1

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
