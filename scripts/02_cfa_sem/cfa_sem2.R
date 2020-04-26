# This script implements the SEMs with POLICY as DV and runs the respective Bayesian model selection
# => sample needs to be selected ('sel') in cfa_mms.R

# load measurement models
source("scripts/02_cfa_sem/cfa_mms.R")

# SEMs
p_res <- F

pdf(file=paste("output/", sel, "/m_sem2.pdf", sep=""), width=8, height=3)

par(mfrow=c(1,4))

size1 <- 9
size2 <- 18
ms <- c(4,3,4,3)

str1 <- paste(
  m_strings$policy,
'
POLICY ~ PRISK + PBENE
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str1)
m1 <- sem(str1, data=d, std.lv=T, estimator=sel_est)
fact1 <- predict(m1)

s1 <- semPaths(m1, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s1$graphAttributes$Edges$labels <- gsub("0.", ".", s1$graphAttributes$Edges$labels, fixed=T)
s1$graphAttributes$Edges$labels <- gsub("1.00", "1", s1$graphAttributes$Edges$labels, fixed=T)

s1$graphAttributes$Edges$curve[11] <- 2

ind <- 5:6
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



# Run Bayesian model selection

d <- get(paste("d_", sel, sep=""))

library(BayesFactor)
library(rstanarm)

factors <- as.data.frame(na.omit(factors))
pred <- cbind(d[match(rownames(factors), d$pid),], factors)

predictors <- c("PRISK", "PBENE", "DREAD", "UNKNO", "TRUST", "KNobj", "KNsub", "PROGR", "age", "sex", "polatt", "education", "occupation", "ehs")

use_dat <- pred[,predictors]
rem_ind <- which(rowSums(apply(use_dat, c(1,2), is.na)) > 0)
pred <- pred[-rem_ind,]
print(paste("Removed", length(rem_ind), "NAs"))

f3 <- as.formula(paste("POLICY ~", paste(predictors, collapse=" + ")))
lm3 <- generalTestBF(f3,
                     data = pred,
                     whichModels = "top")

bfs_both <- NULL
for (i in 3) {
  
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

bfs <- bfs[order(bfs$bf, decreasing=T),"bf", drop = F]
bfs <- bfs * -1

pcs <- rep("lightgrey", nrow(bfs))
pcs[which(is.element(rownames(bfs), colnames(factors)))] <- cols[1]
pcs[is.element(rownames(bfs), c("PRISK", "PBENE"))] <- cols[2]

rownames(bfs) <- gsub("polatt", "political att.", rownames(bfs))
rownames(bfs) <- gsub("ehs", "EHS", rownames(bfs))

if (sel == "w1") {
  xmin <- -25
  xmax <- 350
}
if (sel == "w2_cross") {
  xmin <- -10
  xmax <- 120
}
if (sel == "w2_long") {
  xmin <- -10
  xmax <- 20
}

par(mar=c(3,5,4,1), mgp=c(1.5,0.5,0))
b <- barplot(bfs[,1], horiz=T, las=1, col=pcs, border="white", xlim=c(xmin, xmax), density=c(NA), xlab="Bayes Factor", xaxt="n", cex.names=.9, cex.lab=.9, names=rownames(bfs), main="")
if (sel == "w1") text(160, 19, "Predictors of POLICY", xpd=T)
if (sel == "w2_cross") text(60, 19, "Predictors of POLICY", xpd=T)
if (sel == "w2_long") text(0, 19, "Predictors of POLICY", xpd=T)
if (sel == "w1") axis(1, at=c(-25,0,50,150,250,350), cex.axis=.6)
if (sel == "w2_cross") axis(1, at=c(-10,0,40,80,120), cex.axis=.6)
if (sel == "w2_long") axis(1, at=seq(-10,20,by=5), cex.axis=.6)
abline(h=b[1,]-.75, lwd=.5, col="lightgrey", xpd=T)


abline(v=0, lwd=0.5)

if (sel == "w1") text(-170,20.6, "B", xpd=T, cex=1.7)
if (sel == "w2_cross") text(-60,20.6, "B", xpd=T, cex=1.7)
if (sel == "w2_long") text(-22,20.6, "B", xpd=T, cex=1.7)



dev.off()

system(paste("pdfcrop --margins '0 0 -4 0' output/", sel, "/m_sem2.pdf output/", sel, "/m_sem2_cr.pdf", sep=""))


round(bfs, 0)[nrow(bfs):1,, drop=F]
