# This script computes the cross-sectional and longitudinal changes and estimates whether these changes were systematically different from 0 (for any of the four experimental groups)

library(beanplot)
library(rstanarm)
library(BEST)

do_model <- T

load(file="data/clean/5g_data.Rdata")

sel <- "w2_long_change"

d_sel <- get(paste("d_", sel, sep=""))
v_sel <- c("risk_5g", "bene_pers", "bene_soc", "bene_econ",
           "policy_acc", "policy_vote", "policy_regul", "policy_resea",
           "sc_dread", "sc_unkno", "trust", "sc_kns", "sc_kno",
           "ehs", "polatt", "sc_progr")


levels(d_sel$cond) <- gsub("A_mgmtsum", "Group A:\nSummary", levels(d_sel$cond))
levels(d_sel$cond) <- gsub("B_pressrel", "Group B:\nPress release", levels(d_sel$cond))
levels(d_sel$cond) <- gsub("C_keypoints", "Group C:\nKey points", levels(d_sel$cond))
levels(d_sel$cond) <- gsub("D_control", "Group D:\nControl", levels(d_sel$cond))

lms_file <- "data/local/lms.Rdata"

lms <- list(long = list(), cross = list())
for (i in 1:length(v_sel)) {
  cat(paste("------------------------", v_sel[i], sep="\n"))
  
  # get cross-sectional diffs
  r_w1 <- na.omit(d_w1[[v_sel[i]]])
  r_w2_cross <- na.omit(d_w2_cross[[v_sel[i]]])
  dat <- data.frame(y = c(r_w1, r_w2_cross),
                    gr = c(rep("w1", length(r_w1)), rep("w2", length(r_w2_cross))))
  
  factors <- read.csv(paste("data/clean/w2_long_change_factors.csv"), row.names=1)
  pred <- cbind(d_sel[match(rownames(factors), d_sel$pid),], factors)
  pred$cond <- relevel(pred$cond, ref="Group D:\nControl")
  
  predictors <- c("cond")
  use_dat <- pred[,predictors, drop = F]
  rem_ind <- which(rowSums(apply(use_dat, c(1,2), is.na)) > 0)
  if (length(rem_ind) > 0) pred <- pred[-rem_ind,]
  print(paste("Removed", length(rem_ind), "NAs"))
  
  f <- as.formula(paste(v_sel[i], " ~ ", paste(predictors, collapse=" + ")))
  
  print(round(tapply(pred[,v_sel[i]], pred$cond, mean, na.rm=T), 2))
  

  if (do_model == F) {
    
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
      data = dat,
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
  cat("...")
  
  
  mcmc2 <- as.data.frame(lm_mdiff_rstan)[,1:2]
  out2 <- t(round(rbind(mean=apply(mcmc2, 2, mean),
                        apply(mcmc2, 2, HDInterval::hdi)), 2))
  print(out2)
  
}
save(lms, file=lms_file)




# generate the figure
v_sel <- c("risk_5g", "bene_pers", "bene_soc", "bene_econ", "policy_acc", "policy_vote", "policy_regul", "policy_resea")
pts <- c("Perceived risk", "Perceived benefit (personal)", "Perceived benefit (society)", "Perceived benefit (economy)", "Acceptability of risk", "Voting intention", "Need for more regulation", "Need for more research")

pdf(paste("output/", sel, "/change.pdf", sep=""), height=5, width=11)
par(mfrow=c(2,4), mar=c(3,4.8,2,1), mgp=c(2.5,1.5,0), cex.main=1.5)

for (i in 1:length(v_sel)) {
  
  # get cross-sectional diffs
  r_w1 <- na.omit(d_w1[[v_sel[i]]])
  r_w2_cross <- na.omit(d_w2_cross[[v_sel[i]]])
  dat <- data.frame(y = c(r_w1, r_w2_cross),
                    gr = c(rep("w1", length(r_w1)), rep("w2", length(r_w2_cross))))
  mdiff <- mean(r_w2_cross) - mean(r_w1)
  
  # get longitudinal diffs
  curr <- tapply(d_sel[,v_sel[i]], d_sel$cond, list)
  curr <- rev(curr)
  
  if (is.element(i, 1:4)) pc <- cols[2]
  if (is.element(i, 5:8)) pc <- cols[3]
  
  plot(1, xlim=c(-100,100), ylim=c(1,4.5), type="n", frame=F, xlab="", ylab="", xaxt="n", yaxt="n")
  abline(v=mdiff, lwd=3, col="blue", lty=1)

  b1 <- beanplot(curr, main=pts[i], bw=5, what=c(0,1,0,1), method="jitter", jitter=.1, side="second", boxwex=1, ll=.05, beanlinewd=0.5, border=0, xlab="", ylim=c(-100,100), col="white", las=1, cex.axis=.8, cex.lab=.8, horizontal=T, xaxt="n", yaxt="n", type="n", add=T, cutmin=-100, cutmax=100)
    
  b1 <- beanplot(curr, main=pts[i], bw=5, what=c(0,1,0,1), method="jitter", jitter=.1, side="second", boxwex=1, ll=.05, beanlinewd=0.5, border=0, xlab="", ylim=c(-100,100), col=pc, las=1, cex.axis=.8, cex.lab=.8, horizontal=T, xaxt="n", yaxt="n", type="n", add=T, cutmin=-100, cutmax=100)
  
  axis(1, at=seq(-100, 100, length.out=5), cex.axis=1, mgp=c(3,.5,0))
  if (i != 6) axis(1, c("Lower", "No change", "Higher"), at=seq(-100, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
  if (i == 6) axis(1, c("More contra", "No change", "More pro"), at=seq(-100, 100, length.out=3), mgp=c(3,1.5,0), cex.axis=1.2)
  
  if (i == 1 | i == 5) text(x=-170, y=1:4+.25, names(curr),adj=0, cex.axis=1.1, xpd=T)
  
  abline(v=0, lwd=1, col="black", lty=2)

  
  mlength <- .1
  for (l in 1:4) {
    lines(rep(mean(curr[[l]]), 2), c(l-mlength, l+mlength), col="red", lwd=2)
  }
  
}

dev.off()