# This scripts checks for potential order effects

library(BEST)

load(file="data/clean/5g_data.Rdata")

sel <- "w1"
d_sel <- get(paste("d_", sel, sep=""))

factors <- read.csv(file=paste("data/clean/", sel, "_factors.csv", sep=""), row.names=1)

factors <- as.data.frame(na.omit(factors))
d <- cbind(d_sel[match(rownames(factors), d_sel$pid),], factors)

check <- c(colnames(factors), "risk_5g")

for (i in 1:length(check)) {
  print(check[i])
  ll <- tapply(d[,check[i]], d$ord, list)
  out1 <- t.test(ll$rp_pf, ll$pf_rp)
  #out2 <- BESTmcmc(ll$rp_pf, ll$pf_rp)
  print(out1)
}