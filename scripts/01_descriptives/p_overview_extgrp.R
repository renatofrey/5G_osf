# This script plots the distributions of the main DVs for the two extreme groups (i.e., 10th and 90th percentile concerning risk perception in w1)

library(beanplot)
library(rstanarm)

load(file="data/clean/5g_data.Rdata")

sel <- "w1"
d <- get(paste("d_", sel, sep=""))
if (sel == "w1") v <- v[names(v) != "rep"]

# define ranges
if (!grepl("long", sel)) xlim <- c(0,100) else xlim <- c(-100,100)
br1 <- seq(xlim[1]-.5, xlim[2]+.5, length.out=25)
br2 <- seq(xlim[1], xlim[2], length.out=101)

d_conds <- subset(d, ext_grp != "none")


pdf(paste("output/", sel, "/dist_main_extgrp.pdf", sep=""), height=5, width=11)
par(mfrow=c(2,4), mar=c(3,4,2,1))
for (i in 1:8) {
  if (i == 1) {sel_dat <- d_conds[,"risk_5g"]; pt <- "Perceived risk"; pc <- cols[2]; pl <- "A"}
  if (i == 2) {sel_dat <- d_conds[,"bene_pers"]; pt <- "Perceived benefit (personal)"; pc <- cols[2]; pl <- "B"}
  if (i == 3) {sel_dat <- d_conds[,"bene_soc"]; pt <- "Perceived benefit (society)"; pc <- cols[2]; pl <- "C"}
  if (i == 4) {sel_dat <- d_conds[,"bene_econ"]; pt <- "Perceived benefit (economy)"; pc <- cols[2]; pl <- "D"}
  if (i == 5) {sel_dat <- d_conds[,"policy_acc"]; pt <- "Acceptability of risk"; pc <- cols[3]; pl <- "E"}
  if (i == 6) {sel_dat <- d_conds[,"policy_vote"]; pt <- "Voting intention"; pc <- cols[3]; pl <- "F"}
  if (i == 7) {sel_dat <- d_conds[,"policy_regul"]; pt <- "Need for more regulation"; pc <- cols[3]; pl <- "G"}
  if (i == 8) {sel_dat <- d_conds[,"policy_resea"]; pt <- "Need for more research"; pc <- cols[3]; pl <- "H"}
  
  
  if (is.element(i, c(1:8))) {
    h <- hist(sel_dat, plot=F, breaks=br1)
    plot(h, border="white", col=pc, xlim=xlim, main=pt, las=1, xlab="", cex.main=1.5, xaxt="n", ylim=c(0, ceiling(max(h$counts) / 100)*100))
    abline(v=quants, col="red")
    if (!grepl("long", sel)) {
      axis(1, at=seq(0, 100, length.out=5), cex.axis=1, mgp=c(3,.5,0))
      if (i != 6) axis(1, c("Low", "Medium", "High"), at=seq(0, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
      if (i == 6) axis(1, c("Contra", "Unsure", "Pro"), at=seq(0, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
    } else {
      if (i != 6) axis(1, c("Lower", "No change", "Higher"), at=seq(-100, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
      if (i == 6) axis(1, c("More contra", "No change", "More pro"), at=seq(-100, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
    }
  } else frame()
}
dev.off()