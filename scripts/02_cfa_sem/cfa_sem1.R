# This script implements the SEMs with PRISK and PBENE as DVs and runs the respective Bayesian model selection
# => sample needs to be selected ('sel') in cfa_mms.R

# load measurement models
source("scripts/02_cfa_sem/cfa_mms.R")

# SEMs
p_res <- F

pdf(file=paste("output/", sel, "/m_sem1.pdf", sep=""), width=8, height=3)

par(mfrow=c(1,4))

size1 <- 9
size2 <- 18
ms <- c(4,3,4,3)

str1 <- paste(
  m_strings$dread,
  m_strings$unknown,
'
PRISK ~ DREAD + UNKNO
PBENE ~ DREAD + UNKNO
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str1)
m1 <- sem(str1, data=d, std.lv=T, estimator=sel_est)
fact1 <- predict(m1)

s1 <- semPaths(m1, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s1$graphAttributes$Edges$labels <- gsub("0.", ".", s1$graphAttributes$Edges$labels, fixed=T)
s1$graphAttributes$Edges$labels <- gsub("1.00", "1", s1$graphAttributes$Edges$labels, fixed=T)

s1$graphAttributes$Edges$edge.label.position[1:5] <- .7
s1$graphAttributes$Edges$edge.label.position[6:10] <- .7
s1$graphAttributes$Edges$edge.label.position[12:13] <- .15

ind <- 11:14
s1$graphAttributes$Edges$color[ind] <- "black"
s1$graphAttributes$Edges$label.font[ind] <- 2

s1$graphAttributes$Edges$label.cex <- rep(1.7, length(s1$graphAttributes$Edges$labels))
s1$graphAttributes$Edges$label.cex[ind] <- 2.2

s1$graphAttributes$Nodes$label.cex <- 1.2

plot(s1)
fitm <- fitmeasures(m1)
info <- paste("CFI =", round(fitm["cfi"], 2), 
              "| TLI =", round(fitm["tli"], 2),
              "| RMSEA =", round(fitm["rmsea"], 2), sep=" ")
if (!is.na(fitm['bic'])) info <- paste(info, "| BIC =", round(fitm["bic"], 0))
text(0, -1.275, info, xpd=T, cex=.9)
text(-1.1,1.25, "A", xpd=T, cex=1.7)

str2 <- paste(
  m_strings$trust,
  m_strings$knowledge, '
PRISK ~ TRUST + KNobj + KNsub
PBENE ~ TRUST + KNobj + KNsub
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str2)
m2 <- sem(str2, data=d, std.lv=T, estimator=sel_est)
fact2 <- predict(m2)

s2 <- semPaths(m2, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s2$graphAttributes$Edges$labels <- gsub("0.", ".", s2$graphAttributes$Edges$labels, fixed=T)
s2$graphAttributes$Edges$labels <- gsub("1.00", "1", s2$graphAttributes$Edges$labels, fixed=T)

s2$graphAttributes$Edges$edge.label.position[1:7] <- .7
s2$graphAttributes$Edges$edge.label.position[c(8,9,10)] <- .15

ind <- 8:13
s2$graphAttributes$Edges$color[ind] <- "black"
s2$graphAttributes$Edges$label.font[ind] <- 2

s2$graphAttributes$Edges$curve[c(18,20)] <- 1.9
s2$graphAttributes$Edges$curve[19] <- 3

s2$graphAttributes$Edges$label.cex <- rep(1.7, length(s2$graphAttributes$Edges$labels))
s2$graphAttributes$Edges$label.cex[ind] <- 2.2

s2$graphAttributes$Nodes$label.cex <- 1.2

plot(s2)
fitm <- fitmeasures(m2)
info <- paste("CFI =", round(fitm["cfi"], 2), 
              "| TLI =", round(fitm["tli"], 2),
              "| RMSEA =", round(fitm["rmsea"], 2), sep=" ")
if (!is.na(fitm['bic'])) info <- paste(info, "| BIC =", round(fitm["bic"], 0))
text(0, -1.275, info, xpd=T, cex=.9)
text(-1.1,1.25, "B", xpd=T, cex=1.7)


str3 <- paste(
  m_strings$progress, '
PRISK ~ PROGR
PBENE ~ PROGR
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str3)
m3 <- sem(str3, data=d, std.lv=T, estimator=sel_est)
fact3 <- predict(m3)

s3 <- semPaths(m3, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s3$graphAttributes$Edges$labels <- gsub("0.", ".", s3$graphAttributes$Edges$labels, fixed=T)
s3$graphAttributes$Edges$labels <- gsub("1.00", "1", s3$graphAttributes$Edges$labels, fixed=T)

s3$graphAttributes$Edges$edge.label.position[1:3] <- .7

ind <- c(4,5)
s3$graphAttributes$Edges$color[ind] <- "black"
s3$graphAttributes$Edges$label.font[ind] <- 2

s3$graphAttributes$Edges$label.cex <- rep(1.7, length(s3$graphAttributes$Edges$labels))
s3$graphAttributes$Edges$label.cex[ind] <- 2.2

s3$graphAttributes$Nodes$label.cex <- 1.2

plot(s3)
fitm <- fitmeasures(m3)
info <- paste("CFI =", round(fitm["cfi"], 2), 
              "| TLI =", round(fitm["tli"], 2),
              "| RMSEA =", round(fitm["rmsea"], 2), sep=" ")
if (!is.na(fitm['bic'])) info <- paste(info, "| BIC =", round(fitm["bic"], 0))
text(0, -1.275, info, xpd=T, cex=.9)
text(-1.1,1.25, "C", xpd=T, cex=1.7)



# Run Bayesian model selection

d <- get(paste("d_", sel, sep=""))

library(BayesFactor)
library(rstanarm)

factors <- as.data.frame(na.omit(factors))
pred <- cbind(d[match(rownames(factors), d$pid),], factors)

predictors <- c("DREAD", "UNKNO", "TRUST", "KNobj", "KNsub", "PROGR", "age", "sex", "polatt", "education", "occupation", "ehs")

use_dat <- pred[,predictors]
rem_ind <- which(rowSums(apply(use_dat, c(1,2), is.na)) > 0)
pred <- pred[-rem_ind,]
print(paste("Removed", length(rem_ind), "NAs"))

f1 <- as.formula(paste("risk_5g ~", paste(predictors, collapse=" + ")))
lm1 <- generalTestBF(f1,
                     data = pred,
                     whichModels = "top")

f2 <- as.formula(paste("PBENE ~", paste(predictors, collapse=" + ")))
lm2 <- generalTestBF(f2,
                     data = pred,
                     whichModels = "top")

bfs_both <- NULL
for (i in 1:2) {
  
  sel_mod <- get(paste("lm", i, sep=""))
  
  pt <- "Predictors of "
  if (i == 1) pt <- paste(pt, "perceived risk (PRISK)", sep="")
  if (i == 2) pt <- paste(pt, "perceived benefit (PBENE)", sep="")
  if (i == 3) pt <- paste(pt, "policy-related attitudes (POLICY)", sep="")
  
  bfs <- sel_mod@bayesFactor
  rownames(bfs) <- sapply(rownames(bfs), function(x) {predictors[which(!is.element(predictors, strsplit(x, split=" + ", fixed=T)[[1]]))]})

  if (i == 1) bfs_both <- bfs[,1, drop = F]
  if (i == 2) bfs_both <- cbind(bfs_both,
                                bfs[match(rownames(bfs_both), rownames(bfs)),1, drop =F])
  
}

bfs_both <- bfs_both[order(rowMeans(bfs_both), decreasing=T),]
bfs_both <- bfs_both * -1

pcs <- rep("lightgrey", nrow(bfs_both))
pcs[which(is.element(rownames(bfs_both), colnames(factors)))] <- cols[1]
pcs <- rep(pcs, each=2)

rownames(bfs_both) <- gsub("polatt", "political att.", rownames(bfs_both))
rownames(bfs_both) <- gsub("ehs", "EHS", rownames(bfs_both))

if (sel == "w1") {
  xmin <- -25
  xmax <- 150
}
if (sel == "w2_cross") {
  xmin <- -7
  xmax <- 40
}
if (sel == "w2_long") {
  xmin <- -10
  xmax <- 10
}

par(mar=c(3,5,4,1), mgp=c(1.5,0.5,0))
b <- barplot(t(bfs_both)[2:1,], beside=T, horiz=T, las=1, col=pcs, border="white", xlim=c(xmin, xmax), main="", density=c(50,NA), space=c(0,.5), xlab="Bayes Factor", xaxt="n", cex.names=.9, cex.lab=.9)
if (sel == "w1") axis(1, at=c(-25,0,50,100,150), cex.axis=.6)
if (sel == "w2_cross") axis(1, at=c(-5,0,10,20,30,40), cex.axis=.6)
if (sel == "w2_long") axis(1, at=c(-10,-5,0,5,10), cex.axis=.6)
abline(h=b[1,]-.75, lwd=.5, col="lightgrey")
legend("top", title="Predictors of", c("PRISK", "PBENE"), density=c(NA,50),  box.lty=0, horiz=T, inset=-.22, xpd=T, fill=cols[1], col=cols[1], border=0)

abline(v=0, lwd=0.5)

if (sel == "w1") text(-70,36.8, "D", xpd=T, cex=1.7)
if (sel == "w2_cross") text(-20,36.8, "D", xpd=T, cex=1.7)
if (sel == "w2_long") text(-20,36.8, "D", xpd=T, cex=1.7)



dev.off()

round(bfs_both, 0)[nrow(bfs_both):1,]
