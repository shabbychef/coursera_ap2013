# Final
#
# * Tue Dec 03 2013 12:41:53 PM Steven E. Pav <steven@cerebellumcapital.com>
#

fdisp <- function(x) {
	cat("# ",x,"\n")
}

params <- list(gamma=2,mu=0.08,sigma=0.20,se=0.04)

# Question 20#FOLDUP
fq2 <- function(myp=params) {
	se <- myp$se
	w <- myp$mu / (myp$gamma * (se^2 + myp$sigma^2))
}
resu <- fq2()
fdisp(resu)
# 0.9615
#UNFOLD


#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
