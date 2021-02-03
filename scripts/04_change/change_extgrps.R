# This script runs the same analyses as change.R but only for the two extreme groups (i.e., 10th and 90th percentile concerning risk perception in w1)

# ===> This is a highly exploratory analysis, beware of the smaller sample size!

library(beanplot)
library(rstanarm)
library(BEST)

do_model <- F

load(file="data/clean/5g_data.Rdata")

sel <- "w2_long_change"

d_sel <- get(paste("d_", sel, sep=""))
v_sel <- c("risk_5g", "bene_pers", "bene_soc", "bene_econ", "policy_acc", "policy_vote", "policy_regul", "policy_resea")
pts <- c("Perceived risk", "Perceived benefit (personal)", "Perceived benefit (society)", "Perceived benefit (economy)", "Acceptability of risk", "Voting intention", "Need for more regulation", "Need for more research")

d_sel <- subset(d_sel, ext_grp == "high")

levels(d_sel$cond) <- gsub("A_mgmtsum", "Group A:\nSummary", levels(d_sel$cond))
levels(d_sel$cond) <- gsub("B_pressrel", "Group B:\nPress release", levels(d_sel$cond))
levels(d_sel$cond) <- gsub("C_keypoints", "Group C:\nBullet points", levels(d_sel$cond))
levels(d_sel$cond) <- gsub("D_control", "Group D:\nControl", levels(d_sel$cond))

pdf(paste("output/", sel, "/change_extrgrp.pdf", sep=""), height=5, width=11)
par(mfrow=c(2,4), mar=c(3,4.8,2,1), mgp=c(2.5,1.5,0), cex.main=1.5)

p_bw <- 5

lms <- list(long = list(), cross = list())
for (i in 1:length(v_sel)) {
  cat(paste("---------------------------------------------", v_sel[i], sep="\n"))
  
  # get cross-sectional diffs
  r_w1 <- na.omit(d_w1[[v_sel[i]]])
  r_w2_cross <- na.omit(d_w2_cross[[v_sel[i]]])
  dat2 <- data.frame(y = c(r_w1, r_w2_cross),
                     gr = c(rep("w1", length(r_w1)), rep("w2", length(r_w2_cross))))
  mdiff <- mean(r_w2_cross) - mean(r_w1)
  
  # get longitudinal diffs
  curr <- tapply(d_sel[,v_sel[i]], d_sel$cond, list)
  curr <- rev(curr)
  
  factors <- read.csv(paste("data/clean/w2_long_change_factors.csv"), row.names=1)
  pred <- cbind(d_sel[match(rownames(factors), d_sel$pid),], factors)
  pred$cond <- relevel(pred$cond, ref="Group D:\nControl")
  
  predictors <- c("cond")
  use_dat <- pred[,predictors, drop = F]
  rem_ind <- which(rowSums(apply(use_dat, c(1,2), is.na)) > 0)
  if (length(rem_ind) > 0) pred <- pred[-rem_ind,]
  print(paste("Removed", length(rem_ind), "NAs"))
  
  f <- as.formula(paste(v_sel[i], " ~ ", paste(predictors, collapse=" + ")))
  
  print(round(tapply(pred[,v_sel[i]], pred$cond, mean), 2))
  #print(summary(lm(f, data=pred)))
  
  lms_file <- "data/local/lms_extrgr.Rdata"
  if (do_model == T) {
      # estimate diffs as a function of experimental manipulation
      lm_rstan <- stan_lm(
        formula = f,
        data = pred,
        cores = 3,
        chains = 3,
        iter = 2000,
        prior = NULL
      )
      lms[["long"]][[i]] <- lm_rstan
      names(lms[["long"]])[i] <- v_sel[i]
      
      # estimate mean diff in cross-sectional sample
      lm_mdiff_rstan <- stan_lm(
        formula = y ~ gr,
        data = dat2,
        cores = 3,
        chains = 3,
        iter = 2000,
        prior = NULL
      )
      lms[["cross"]][[i]] <- lm_mdiff_rstan
      names(lms[["cross"]])[i] <- v_sel[i]
    } else {
      load(lms_file)
      lm_rstan <- lms[["long"]][[i]]
      lm_mdiff_rstan <- lms[["cross"]][[i]]
    }
  
    mcmc1 <- as.data.frame(lm_rstan)
    mcmc1 <- cbind(mcmc1[,1, drop=F], mcmc1[,2:4] + mcmc1[,1])
    out1 <- t(round(rbind(mean=apply(mcmc1, 2, mean),
                          apply(mcmc1, 2, HDInterval::hdi)), 2))
    print(out1)
    
    mcmc2 <- as.data.frame(lm_mdiff_rstan)[,1:2]
    out2 <- t(round(rbind(mean=apply(mcmc2, 2, mean),
                          apply(mcmc2, 2, HDInterval::hdi)), 2))
    print(out2)
    


  
  if (i <= 4) pc <- cols[2] else pc <- cols[3]
  
  plot(1, xlim=c(-100,100), ylim=c(0.5,4.5), type="n", frame=F, xlab="", ylab="", xaxt="n", yaxt="n")
  abline(v=mdiff, lwd=3, col="blue", lty=1)
  
  b1 <- beanplot(curr, main=pts[i], bw=p_bw, what=c(0,1,0,1), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=0.5, border=0, xlab="", ylim=c(-100,100), col=pc, las=1, cex.axis=.8, cex.lab=.8, horizontal=T, xaxt="n", yaxt="n", type="n", add=T, cutmin=-100, cutmax=100)
  
  axis(1, at=seq(-100, 100, length.out=5), cex.axis=1, mgp=c(3,.5,0))
  if (i != 6) axis(1, c("Lower", "No change", "Higher"), at=seq(-100, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
  if (i == 6) axis(1, c("More contra", "No change", "More pro"), at=seq(-100, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
  
  if (i == 1 | i == 5) axis(2, at=1:4, names(curr), las=1, mgp=c(1,.2,0), tick=F, hadj=0.5, pos=-133, cex.axis=1.1)
  
  #abline(v=0, lwd=2, col="black")
  
  
  mlength <- .1
  for (l in 1:4) {
    lines(rep(mean(curr[[l]]), 2), c(l-mlength, l+mlength), col="red", lwd=2)
  }
  
}
if (do_model == T) save(lms, file=lms_file)

dev.off()