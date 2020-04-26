# This script plots the distributions of perceived health risks

library(beanplot)
library(rstanarm)

load(file="data/clean/5g_data.Rdata")

sel <- "w1"

v_sel <- v$healthrisks
d_sel <- get(paste("d_", sel, sep=""))[,v_sel]

d_sel <- d_sel[,order(colMeans(d_sel[,v_sel]), decreasing=F)]
pts <- colnames(d_sel)
pts <- gsub("healthrisk_5g", "5G", pts)
pts <- gsub("healthrisk_4g", "4G", pts)
pts <- gsub("healthrisk_traffic", "Traffic", pts)
pts <- gsub("healthrisk_alc", "Drinking", pts)
pts <- gsub("healthrisk_smoke", "Smoking", pts)
pts <- gsub("healthrisk_exer", "Insufficient\nexercise", pts)
pts <- gsub("healthrisk_diet", "Unhealthy diet", pts)
pts <- gsub("healthrisk_vacc", "Vaccinations", pts)
pts <- gsub("healthrisk_airpol", "Air pollution", pts)

pdf(paste("output/", sel, "/healthrisks.pdf", sep=""), height=7, width=7)
par(mar=c(3,6,2,3), mgp=c(0,0,0), cex.main=1.5)

p_bw <- 7

pc <- "orange"

b1 <- beanplot(d_sel, bw=p_bw, what=c(0,1,0,1), method="jitter", jitter=.05, boxwex=1, ll=.1, border=0, xlab="", ylim=c(0,100), col=pc, cex.axis=1, cex.lab=.8, cex.main=1.1, horizontal=T, frame.plot=F, xaxt="n", names=pts, las=1, tick=F, cutmax=100, cutmin=0, main="Perception of health risks")

axis(1, at=seq(0, 100, length.out=5), cex.axis=1, mgp=c(3,.5,0))
axis(1, c("Low", "Medium", "High"), at=seq(0, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)  

#abline(v=50, lwd=0.5, col="black")


mlength <- .3
for (l in 1:ncol(d_sel)) {
  lines(rep(mean(d_sel[,l]), 2), c(l-mlength, l+mlength), col="red", lwd=2)
}

dev.off()

# library(BEST)
# best <- BESTmcmc(d_w1$healthrisk_4g, d_w1$healthrisk_5g,
#                  parallel=T, verbose=T)
t.test(d_w1$healthrisk_4g, d_w1$healthrisk_5g)

print(round(apply(d_sel[,v$healthrisks], 2, mean), 2))