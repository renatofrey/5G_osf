library(gplots)
library(viridis)

load("data/clean/5g_data.Rdata")
load("data/clean/bas.Rdata")

newlabels <- function(x, br=T) {
  x <- gsub("PRISK", "Perceived--risk", x)
  x <- gsub("PBENE", "Perceived--benefit", x)
  x <- gsub("DREAD", "Dread (5G)", x)
  x <- gsub("UNKNO", "Unknown risk (5G)", x)
  x <- gsub("ehs", "Electrom. hypersensitivity", x)
  x <- gsub("polatt", "Political--attitude", x)
  x <- gsub("KNsub", "Subjective knowledge (5G)", x)
  x <- gsub("KNobj", "Objective knowledge (5G)", x)
  x <- gsub("sexmale", "Gender--(male)", x)
  x <- gsub("sex", "Sex", x)
  x <- gsub("PROGR", "Preference for--progression", x)
  x <- gsub("TRUST", "Trust (5G)", x)
  x <- gsub("education.L", "Education__", x)
  x <- gsub("age", "Age__", x)
  x <- gsub("occupationemployed", "Occupation__", x)
  if (br == T) x <- sapply(x, function(y) {gsub("--", "\n", y)})
  if (br == T) x <- sapply(x, function(y) {gsub("__", "\n", y)})
  if (br == F) x <- sapply(x, function(y) {gsub("--", " ", y)})
  if (br == F) x <- sapply(x, function(y) {gsub("__", "", y)})
  return(x)
}


pdf(file=paste("output/w1/fig2.pdf", sep=""), height=4, width=10)

m <- rbind(matrix(rep(c(1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5), 8), nrow=8, byrow=T),
           c(6,6,6,rep(7,12),rep(8,4)))
layout(m)

BFs <- NULL

for (dv in c("PRISK", "PBENE", "POLICY")) {
  
  tab1 <- bas_ivs[[paste("w1_", dv, sep="")]]
  tab1 <- tab1[!grepl("Intercept", rownames(tab1)),]
  
  ind_occ <- !grepl("occupation", rownames(tab1)) | rownames(tab1) == "occupationemployed"
  tab1 <- tab1[ind_occ,]
  
  ind_edu <- !grepl("education.", rownames(tab1)) | rownames(tab1) == "education.L"
  tab1 <- tab1[ind_edu,]
  
  if (dv == "PRISK") tab1 <- tab1[order(tab1$bf_full_log, decreasing=T),]
  if (dv != "POLICY") tab1 <- rbind(tab1, "PRISK" = 0, "PBENE" = 0)
  if (dv == "PRISK") ind_v <- rownames(tab1) else tab1 <- tab1[ind_v,]
  
  cat(dv)
  print(round(tab1[,4:ncol(tab1)], 0))

  means <- as.matrix(rbind(tab1$mean))
  cil <- as.matrix(rbind(tab1$mean - tab1$sd))
  ciu <- as.matrix(rbind(tab1$mean + tab1$sd))
  
  # store BFs
  BFs <- cbind(BFs, tab1$bf_full_log)
  
  labels <- newlabels(rownames(tab1), br=F)
  
  colnames(means) <- rep("", ncol(means))
  
  # revert order for vertical plotting?
  if (T) {
    nr <- nrow(means)
    nc <- ncol(means)
    means <- means[nr:1,nc:1]
    cil <- cil[nr:1,nc:1]
    ciu <- ciu[nr:1,nc:1]
  }
  
  mm <- c(3,0,2,0)
  par(mar=mm)
  
  yrange1 <- range(c(means, cil, ciu), na.rm=T)
  yrange2 <- c(floor(yrange1[1]*10)/10, ceiling(yrange1[2]*10)/10)
  if (dv != "POLICY") yrange2 <- c(-.4, .4) else yrange2 <- c(-.4, .4)
  
  if (dv == "PRISK") {frame(); par(mfg=c(1,2)); pm <- "A: Perceived risk"}
  if (dv == "PBENE") {par(mfg=c(1,3)); pm <- "B: Perceived benefit"}
  if (dv == "POLICY") {par(mfg=c(1,4)); pm <- "C: Policy-related attitudes"}
 
  # set colors
  pc <- rep(viridis(1, begin=.3), nc)
  ind_pers <- as.numeric(which(sapply(rownames(tab1), function(x) {is.element(x, c("ehs", "age", "occupationemployed", "education.L", "polatt", "PROGR", "sexmale"))}) == TRUE))
  pc[ind_pers] <- viridis(1, begin=.6)
  pc[rownames(tab1) == "PRISK"] <- gray(.3)
  pc[rownames(tab1) == "PBENE"] <- gray(.3)
  pc <- rev(pc)

  if (nr == 1) {
    pd <- rep(NA, length(pc))
    pd[ind_pers] <- 40
    pd <- rev(pd)
  } else pd <- c(50,100)
  
  if (dv == "PBENE") xl <- "Model-averaged coefficients" else xl <- "Model-averaged coefficients"
  b <- barplot2(means, plot.ci=T, ci.l=cil, ci.u=ciu, beside=T, border=F, col=pc, las=1, cex.names=1, names=colnames(means), xlim=c(-.5,.5), mgp=c(1.5,0.5,0), density=pd, main=pm, cex.main=1.4, space=c(0.3,.6), horiz=T, ci.color="darkgrey", xlab=xl)
  
  # clean 0-estimates
  if (is.element(dv, c("PRISK", "PBENE"))) points(c(0,0), b[1:2], pch=15, cex=2.5, col="white")
  
  if (nr == 1) ty <- b else ty <- colMeans(b)
  ygap <- (ty[2] - ty[1]) / 2
  ylines <- tail(ty + ygap, -1)

  abline(h=ylines, col="lightgrey", lwd=0.5)
  
  if (dv == "POLICY") ylines <- c(0, ylines)
  
  lines(x=c(0,0), y=c(min(ylines), max(ylines)), lwd=0.5, col="lightgrey")
  
}


# plot predictor labels
par(mfg=c(1,1), mar=mm)
text(x=-.5, y=ty, rev(labels), adj=0, xpd=T, cex=1.1)
#text(x=-.4, -4, "Standardized coef.:", adj=0, xpd=T, font=3)
abline(h=tail(ylines, -1), xpd=T, col="lightgrey", lwd=0.5)

par(mfg=c(1,6), mar=c(0,0,0,0))
frame()

par(mfg=c(1,7), mar=c(0,0,0,0))
frame()
pcs <- c(pc[which(rev(rownames(tab1)) == "TRUST")],
         pc[which(rev(rownames(tab1)) == "ehs")],
         gray(.3))
legend(x=0.32, y=1.2, horiz=F, box.lty=0,
       c("Hazard-related drivers",
         "Person-specific drivers",
         "Perceived risk / benefit as predictors"),
       col=pcs, border=pcs, cex=1.05,
       fill=pcs, density=c(NA,40,NA))


# plot BFs
par(mfg=c(1,5), mar=mm)
p_BFs <- t(BFs)
p_BFs <- p_BFs[nrow(p_BFs):1,ncol(p_BFs):1]
cutmin <- -30
p_BFs[p_BFs < cutmin] <- cutmin

pc2 <- c("darkred")#, "darkgreen", "darkblue")
pd2 <- c(50, 0, 200)
xmax <- 200
par(lwd=0.75)
b2 <- barplot(p_BFs, beside=T, horiz=T, border=c(pc2,pc2,pc2), xlim=c(-50,xmax), xpd=F, las=1, main="D: Importance of predictors", xaxt="n", col=pc2, density=pd2, xlab=expression('Bayes Factor (log'[10]*')'), mgp=c(1.7,0,0), cex.main=1.4)
axis(1, at=c(cutmin,0,60,120,180), mgp=c(0,.5,0))
abline(v=cutmin, lty=2, col="lightgrey", lwd=.75)

ty <- colMeans(b2)
ygap <- (ty[2] - ty[1]) / 2
ylines <- tail(c(ty + ygap), -1)
abline(h=ylines, col="lightgrey", lwd=0.5)
abline(v=0)
lines(x=c(0,0), y=c(min(ylines), max(ylines)), lwd=0.5, col="lightgrey")

par(mfg=c(1,8), mar=c(0,0,0,0))
frame()
#abline(h=1, xpd=T, lwd=0.5)
legend(x=0, y=1.2, col=pc, fill=pc2, density=rev(pd2), border=pc2, c("Perceived risk as outcome", "Perceived benefit as outcome", "Policy-related attitud. as outcome"), box.lty=0, xpd=T, cex=1.05)


dev.off()

raftery <- c("weak"=1, "positive"=3, "strong"=20, "very strong"=150)
print(round(log10(raftery), 2))