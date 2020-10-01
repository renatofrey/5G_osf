library(BAS)
library(gplots)

# load data
load(file="data/clean/5g_data.Rdata")

bas_models <- list()
bas_ivs <- list()

for (sel in c("w1", "w2_cross", "w2_long_change")) {
  
  # get data
  d <- get(paste("d_", sel, sep=""))
  
  # add factor values from CFAs
  factors <- read.csv(paste("data/clean/", sel, "_factors.csv", sep=""), row.names=1)
  factors <- as.data.frame(na.omit(factors))
  d <- cbind(d, factors[match(d$pid, rownames(factors)),]) 
  
  set.seed(123)
  
  # loop through DVs
  for (dv in c("PRISK", "PBENE", "POLICY")) {
    
    # define IVs
    ivs_fa <- c("DREAD", "UNKNO", "TRUST", "KNobj", "KNsub", "PROGR")
    ivs_sc <- c("sc_dread", "sc_unkno", "trust", "sc_kno", "sc_kns", "sc_progr")
    ivs_pr <- c("riskpref", "openness", "digit")
    ivs_soc <- c("age", "sex", "education", "occupation", "ehs")
    
    sc_cors <- cor(d[,c(ivs_fa, ivs_sc, ivs_pr, "sc_bene", "PBENE")], use="complete.obs")
    assign(paste("sc_cors", sel, sep="_"), sc_cors)
    if (sel == "w1") print(round(sc_cors[7:nrow(sc_cors),], 2))
    
    ivs <- c(ivs_fa, ivs_soc)
    
    # add prisk and pbene as direct predictors if DV is POLICY
    if (dv == "POLICY") ivs <- c(ivs, "PRISK", "PBENE")
    
    
    if (sel == "w2_long_change") {
      ivs <- ivs[sapply(ivs, function(x) {!is.element(x, c("age",
                                                           "sex",
                                                           "education",
                                                           "occupation"))
      })]
    }  
    
    use_dat <- d[,c(ivs, dv)]
  
    # remove rows with NAs
    rem_ind <- which(rowSums(apply(use_dat, c(1,2), is.na)) > 0)
    if (length(rem_ind) > 0) use_dat <- use_dat[-rem_ind,]
    cat(paste("Removed", length(rem_ind), "NAs. Remaining N:", nrow(use_dat), " \n"))
    
    # scale
    ind_num <- which(sapply(use_dat, class) == "numeric")
    use_dat[,ind_num] <- apply(use_dat[,ind_num], 2, scale)
  
    # in the longitudinal dataset, the following variables are the values from wave 2 (i.e., no deltas): age, sex, education, occupation
    
    t1 <- Sys.time()
    
    # formula with main effects only
    f <- as.formula(paste(dv, "~", paste(ivs, collapse=" + ")))
    
    mbas <- bas.lm(f,
                   data = use_dat,
                   method = "BAS",
                   modelprior = uniform(),
                   prior = "ZS-null",
                   force.heredity = TRUE,
                   n.models = 10^6
    )
    
    cat(paste("Number of models evaluated:", mbas$n.models, "\n"))
    
    t2 <- Sys.time()
    print(t2-t1)

    n_mod <- mbas$n.models
    
    # assemble table with models
    ind_m <- order(mbas$postprobs, decreasing=T)
    best_m <- ind_m[1]
    tab_models <- data.frame(postprob = mbas$postprobs[ind_m],
                             r2 = mbas$R2[ind_m])
    
    # assemble table with predictors
    coefs <- coef(mbas)
    tab_ivs <- data.frame("mean"=coefs$postmean,
                          "sd"=coefs$postsd,
                          "incl_prob"=coefs$probne0)
    rownames(tab_ivs) <- coefs$namesx
    tab_ivs <- tab_ivs[order(tab_ivs$mean, decreasing=T),]
    
    # compute BFs full model vs. full-pred. / best vs best-pred.
    inds_ivs <- mbas$which
    ns_ivs <- sapply(inds_ivs, length)
    ind_mfull <- which.max(ns_ivs)
    inds_mfull_ivs <- inds_ivs[[ind_mfull]] + 1
    inds_mbest_ivs <- inds_ivs[[best_m]]+1
    
    mfull_prior <- 1 / n_mod
    #mfull_post <- mbas$postprobs[ind_mfull]
    mfull_post <- mbas$postprobs[best_m]
    bf_full <- mfull_post / mfull_prior
    #r2_full <- mbas$R2[ind_mfull]
    r2_full <- mbas$R2[best_m]
    
    tab_ivs$r2_incr <- NA
    tab_ivs$bf_full <- NA
    for (iv in ivs) {
      ind_ivcurrent <- which(grepl(iv, mbas$namesx))[1]
      #inds_ivsother <- setdiff(inds_mfull_ivs, ind_ivcurrent)
      if (is.element(ind_ivcurrent, inds_mbest_ivs)) target <- setdiff(inds_mbest_ivs, ind_ivcurrent) else target <- c(inds_mbest_ivs, ind_ivcurrent)
      
      for (j in 1:length(inds_ivs)) {
        check <- sort(inds_ivs[[j]]) + 1
        #if (length(check) == length(inds_ivsother)) {
        if (length(check) == length(target)) { 
          #if (sum(check != inds_ivsother) == 0) {
          if (sum(check != target) == 0) {
            sel_ind <- j
            break
          }
        }
      }
      
      mcurr_prior <- 1 / n_mod
      mcurr_post <- mbas$postprobs[sel_ind]
      bf_curr <- mcurr_post / mcurr_prior
      r2_curr <- mbas$R2[sel_ind]
      
      if (is.element(ind_ivcurrent, inds_mbest_ivs)) {
        r2_diff <- r2_full - r2_curr
        bf_diff <- bf_full / bf_curr
      } else {
        r2_diff <- r2_curr - r2_full 
        bf_diff <- bf_curr / bf_full
      }
      
      tab_ivs[grepl(iv, rownames(tab_ivs)),"r2_incr"] <- r2_diff
      tab_ivs[grepl(iv, rownames(tab_ivs)),"bf_full"] <- bf_diff
      
      
    }
    tab_ivs$bf_full_log <- log10(tab_ivs$bf_full)
    
    
    
    # plot some intermediate results
    
    curr_file <- paste("output/", sel, "/bas_", dv, ".pdf", sep="")
    cat(paste(curr_file, "\n"))
    
    pdf(file=curr_file, height=4, width=14)
    par(mfrow=c(1,4))
    
    
    
    n_show <- 25
    
    # panel 1: plot model comparison
    par(mar=c(7,5,2,0))
    postprobs <- head(tab_models, n_show)$postprob
    r2s <- head(tab_models, n_show)$r2
    ymax <- ceiling(max(postprobs) * 10^2) / 10^2
    plot(postprobs, frame=F, cex=exp(postprobs),
         pch=21, col="white", bg="black",
         las=1, ylim=c(0, ymax),
         xlab="", ylab="", xaxt="n", yaxt="n", main=paste(dv, ": Bayesian Model Comparison", sep=""))
    
    title(xlab=paste("Model rank out of", n_mod ,"models\n(prior model probability = 1 / ", n_mod, ")", sep=""), mgp=c(3.5,1,0))
    title(ylab="Posterior model probability", mgp=c(4,1,0))
    axis(1, c(1, seq(5, n_show, by=5)))
    axis(2, seq(0, ymax, by=0.1), las=1, mgp=c(9,1,0))
    
    BFs <- round(postprobs / (1 / n_mod), 1)
    text(1, postprobs[1], paste("BF =", BFs[1]), pos=4, cex=.7)
    text(2, postprobs[2], paste("BF =", BFs[2]), pos=4, cex=.7)
    text(3, postprobs[3], paste("BF =", BFs[3]), pos=4, cex=.7)
    
    
    
    
    # panel 2: plot inclusion probabilities
    tab1 <- head(tab_ivs, n_show)
    tab1 <- tab1[!grepl("Intercept", rownames(tab1)),]
    
    tab1$col <- "lightgray"
    tab1$col[which(tab1$incl_prob > .5)] <- "lightblue"
    
    # condense occupation levels (already removed in w2_long)
    if (sum(grepl("occupation", rownames(tab1))) > 0) {
      tab1 <- tab1[-tail(which(grepl("occupation", rownames(tab1))), -1),]
      rownames(tab1)[grepl("occupation", rownames(tab1))] <- "occupation"
    }
    
    par(mar=c(7,5,2,0))
    barplot(tab1$incl_prob, las=2, border=F, names=rownames(tab1), cex.names=.7, mgp=c(1,.5,0), yaxt="n", col=tab1$col, main=paste(dv, ": Importance of predictors", sep=""), ylim=c(0,1))
    abline(h=.5, col="lightgray")
    title(ylab="Posterior inclusion probability", mgp=c(3.5,1,0))
    axis(2, seq(0,1,by=.25), las=2)
    
    
    
    # panel 3: plot estimates
    tab2 <- tab_ivs[order(tab_ivs$mean, decreasing=T),]
    tab2$col <- "lightgray"
    tab2$col[which(tab2$incl_prob > .5)] <- "lightblue"
    
    # remove occupation (i.e., multiple levels)
    tab2 <- tab2[!grepl("occupation", rownames(tab2)),]
    
    r_sel <- 1:nrow(tab2)
    means <- tab2$mean[r_sel]
    cil <- tab2$mean[r_sel] - tab2$sd[r_sel]
    ciu <- tab2$mean[r_sel] + tab2$sd[r_sel]
    names(means) <- rownames(tab2)[r_sel]
    
    par(mar=c(7,5,2,0))
    yrange <- range(c(means, cil, ciu))
    #yrange2 <- c(floor(yrange[1]*10)/10, ceiling(yrange[2]*10)/10)
    yrange2 <- c(-.4, .4)
    
    b <- barplot2(means, plot.ci=T, ci.l=cil, ci.u=ciu, beside=T, border="white", col=tab2$col, las=2, cex.names=.7, names=names(means), ylim=yrange2, yaxt="n", main=paste(dv, ": Model-Averaged Estimates", sep=""), mgp=c(1,.5,0))
    axis(2, round(seq(yrange2[1], yrange2[2], by=.2), 1), las=2)
    abline(h=0, col="lightgrey")
    title(ylab="Standardized coefficients", mgp=c(3,1,0))
    
    
    
    # panel 4: plot BFs full vs. -1
    tab3 <- tab_ivs[order(tab_ivs$bf_full_log, decreasing=F),]
    tab3 <- tab3[!grepl("Intercept", rownames(tab3)),]
    
    # condense occupation levels (already removed in w2_long)
    if (sum(grepl("occupation", rownames(tab1))) > 0) {
      tab3 <- tab3[-tail(which(grepl("occupation", rownames(tab3))), -1),]
      rownames(tab3)[grepl("occupation", rownames(tab3))] <- "occupation"
    }
    
    par(mar=c(7,8,2,2))
    
    if (sel == "w1") x.lim <- c(-15,330) else x.lim <- c(-15,15)
    x.lim <- c(0, max(tab3$bf_full_log))

    barplot(tab3$bf_full_log, col="darkred", las=1, border=F, horiz=T, xlim=x.lim, names=rownames(tab3), main=paste(dv, ": Importance of predictors", sep=""), xpd=F)
    title(xlab="Bayes Factor", mgp=c(3,1,0))
    abline(v=0, col="lightgrey", lwd=.5)
    
    dev.off()
    
    
    # save extracted tables
    bas_models[[paste(sel, "_", dv, sep="")]] <- tab_models
    bas_ivs[[paste(sel, "_", dv, sep="")]] <- tab_ivs
    
    
    # predictors in best model
    
    mbas$namesx[inds_mbest_ivs]
    
    barplot2(coef(mbas, n.models=1)[[1]],
             plot.ci=T,
             ci.l=coef(mbas, n.models=1)[[1]] - coef(mbas, n.models=1)[[2]],
             ci.u=coef(mbas, n.models=1)[[1]] + coef(mbas, n.models=1)[[2]],
             las=2, xpd=F, cex.names=.6, border=F, ylim=c(-.4,.4), names=mbas$namesx, main=paste(sel, dv))
    abline(h=0, col="lightgrey")
    
  }
}

save(bas_models, bas_ivs, file="data/clean/bas.Rdata")
