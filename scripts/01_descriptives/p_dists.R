# Plot distributions of main DVs

load(file="data/clean/5g_data.Rdata")

library(corrplot)
library(diptest)

for (sel in c("w1", "w2_cross", "w2_long", "w2_long_change")) {
  cat("\n")
  print(sel)
  
  # select data
  d <- get(paste("d_", sel, sep=""))
  if (sel == "w1") v <- v[names(v) != "rep"]
  
  # print some stats for selected main DVs
  sel_v <- c("risk_5g", "bene_pers", "bene_soc", "bene_econ", "policy_acc", "policy_vote", "policy_regul", "policy_resea")
  print(cbind(mean = round(apply(d[,sel_v], 2, mean, na.rm=T), 1),
              sd = round(apply(d[,sel_v], 2, sd, na.rm=T), 1),
              t(round(apply(d[,sel_v], 2, function(x) {prop.table(table(x > 50))}), 2))))
  
  
  # define ranges
  if (!grepl("change", sel)) xlim <- c(0,100) else xlim <- c(-100,100)
  br1 <- seq(xlim[1]-.5, xlim[2]+.5, length.out=25)
  br2 <- seq(xlim[1], xlim[2], length.out=101)
  
  print(xlim)
  
  # plot distributions of main outcome variables
  if (grepl("long", sel)) conds <- 4 else conds <- 0
  
  for (cond in 0:conds) {
    
    if (cond == 0) {
      f_add <- ""
      d_conds <- d
    } else {
      f_add <- paste("_", LETTERS[cond], sep="")
      d_conds <- d[grepl(paste(LETTERS[cond], "_", sep=""), d$cond),]
    }
    
    pdf(paste("output/", sel, "/dists_main", f_add, ".pdf", sep=""), height=5, width=11)
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
  }
  
  names(v)[which(names(v) == "mms")] <- "vars"
  # plot distributions for main groups of variables
  for (k in c("vars", "healthrisks")) {
    
    vars <- v[[k]]
    x <- ceiling(length(vars) / 6)
    
    pdf(paste("output/", sel, "/dists_", k, ".pdf", sep=""), width=12, height=8/4*x)
    
    par(mfrow=c(x, 6), mar=c(4,3,2,1))  
    
    for (c in vars) {
      p_title <- c
      hist(d[,c], border=0, col="blue", breaks=br2, xlim=xlim, main=p_title, las=1, xlab="", cex=.7)
    }
    dev.off()
  }

}