# Plot correlations

load(file="data/clean/5g_data.Rdata")

library(corrplot)
library(diptest)

for (sel in c("w1", "w2_cross", "w2_long", "w2_long_change")) {
  print(sel)
  
  # select data
  d <- get(paste("d_", sel, sep=""))
  if (sel == "w1") v <- v[names(v) != "rep"]
  
  # plot correlations between main variables
  cormat <- cor(d[,v$mms], use="complete.obs")
  l <- colnames(cormat)
  l <- gsub("risk_5g", "risk", l)
  l <- gsub("policy_acc", "policy_accep", l)
  l <- gsub("riskpref", "progr_rpref", l)
  l <- gsub("openness", "progr_open", l)
  l <- gsub("digit", "progr_digit", l)
  colnames(cormat) <- l
  rownames(cormat) <- l
  ind <- c(9:28, 1:8)
  cormat <- cormat[ind,ind]
  
  pdf(paste("output/", sel, "/cors_vars.pdf", sep=""))
  corrplot(cormat, method="color", type="lower", diag=T, is.cor=T, mar=c(0,1,2,1), addCoef.col = "black", number.font=1, number.cex=.4, tl.col="red", cl.cex=1, cl.ratio=0.05, xpd=T)
  dev.off()
  
  # plot correlations between health risks
  health <- d[,v$healthrisks]
  pts <- colnames(health)
  pts <- gsub("healthrisk_5g", "5G", pts)
  pts <- gsub("healthrisk_4g", "4G", pts)
  pts <- gsub("healthrisk_traffic", "Traffic", pts)
  pts <- gsub("healthrisk_alc", "Drinking", pts)
  pts <- gsub("healthrisk_smoke", "Smoking", pts)
  pts <- gsub("healthrisk_exer", "Insufficient\nexercise", pts)
  pts <- gsub("healthrisk_diet", "Unhealthy diet", pts)
  pts <- gsub("healthrisk_vacc", "Vaccinations", pts)
  pts <- gsub("healthrisk_airpol", "Air pollution", pts)
  colnames(health) <- pts
  cormat <- cor(health, use="complete.obs")
  ind <- c(5,4,7,6,9,3,1,2,8)
  cormat <- cormat[ind,ind]
  pdf(file=paste("output/", sel, "/cors_healthrisks.pdf", sep=""))
  #par(cex=.6)
  corrplot(cormat, method="color", type="lower", diag=T, is.cor=T, mar=c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=.7, tl.col="red", cl.cex=1, cl.ratio=0.05)
  dev.off()
  
}