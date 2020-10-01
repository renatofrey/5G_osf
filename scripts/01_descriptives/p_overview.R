# Plot overview of distributions, correlations, etc.

load(file="data/clean/5g_data.Rdata")

library(corrplot)
library(diptest)

for (sel in c("w1", "w2_cross", "w2_long")) {
  print(sel)
  
  # select data
  d <- get(paste("d_", sel, sep=""))
  if (sel == "w1") v <- v[names(v) != "rep"]
  
  # define ranges
  if (!grepl("long", sel)) xlim <- c(0,100) else xlim <- c(-100,100)
  br1 <- seq(xlim[1]-.5, xlim[2]+.5, length.out=25)
  br2 <- seq(xlim[1], xlim[2], length.out=101)
  
  # plot distributions for main groups of variables
  for (i in 1:length(v)) {
    
    vars <- v[[i]]
    x <- ceiling(length(vars) / 5)
    
    pdf(paste("output/", sel, "/dist_", names(v)[i], ".pdf", sep=""), width=7/3*x, height=8)
    
    par(mfcol=c(5, x), mar=c(4,3,2,1))  
    
    for (c in vars) {
      dip_p <- round(dip.test(d[,c])$p.value, 2)
      print(paste(c, "/ pval (DIP) = ", dip_p))
      p_title <- c
      hist(d[,c], border=0, col="blue", breaks=br2, xlim=xlim, main=p_title, las=1, xlab="", cex=.7)
    }
    dev.off()
  }
  
  
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
    
    pdf(paste("output/", sel, "/dist_main", f_add, ".pdf", sep=""), height=5, width=11)
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
  
  
  # plot correlations of main variables that are part of measurement models
  cormat <- cor(d[,v$mms], use="complete.obs")
  
  l <-colnames(cormat)
  l <- gsub("risk_5g", "risk", l)
  l <- gsub("policy_acc", "policy_accep", l)
  l <- gsub("riskpref", "progr_rpref", l)
  l <- gsub("openness", "progr_open", l)
  l <- gsub("digit", "progr_digit", l)
  colnames(cormat) <- l
  rownames(cormat) <- l
  ind <- c(9:28, 1:8)
  cormat <- cormat[ind,ind]
  
  pdf(paste("output/", sel, "/cor_main.pdf", sep=""))
  corrplot(cormat, method="color", type="lower", diag=T, is.cor=T, mar=c(0,1,2,1), addCoef.col = "black", number.font=1, number.cex=.4, tl.col="red", cl.cex=1, cl.ratio=0.05, xpd=T)
  dev.off()
  
  # plot correlations of health risks
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
  pdf(file=paste("output/", sel, "/cor_health.pdf", sep=""))
  #par(cex=.6)
  corrplot(cormat, method="color", type="lower", diag=T, is.cor=T, mar=c(0,0,0,0), addCoef.col = "black", number.font=1, number.cex=.7, tl.col="red", cl.cex=1, cl.ratio=0.05)
  dev.off()
  
  # sociodemographic data
  if (!grepl("long", sel)) {
    
    # for wave 1, also plot sociodem. data separately for high / low extreme groups
    if (sel == "w1") js <- 1:3 else js <- 1
    for (j in js) {
      
      if (j == 1) {
        soc_dat <- d
        soc_add <- ""
      }
      if (j == 2) {
        soc_dat <- subset(d, ext_grp == "low")
        soc_add <- "_low"
      }
      if (j == 3) {
        soc_dat <- subset(d, ext_grp == "high")
        soc_add <- "_high"
      }
      
      pdf(file=paste("output/", sel, "/socdem", soc_add, ".pdf", sep=""), height=8, width=8)
      
      library(viridis)
      pcs <- magma(5, begin=.15, end=.7)
      par(mfrow=c(2,2), mar=c(3,4,2,1))
      
      if (j == 1) ylim=c(0,1600) else ylim=c(0,200)
      soc_dat$sex <- droplevels(soc_dat$sex)
      soc_dat$language <- droplevels(soc_dat$language)
      t1 <- table(soc_dat$sex, soc_dat$language)
      colnames(t1) <- c("German", "French")
      b <- barplot(t1, beside=T, las=1, ylim=ylim, col=pcs[1:2], border=0, yaxt="n", main="Gender and language", space=c(0,0.5), ylab="Frequency", las=1)
      if (j == 1) {
        axis(2, at=seq(ylim[1],ylim[2], by=500), las=1)
        text(rep(c("female", "male"), 2), x=b, y=50, col="lightgrey", font=2, srt=90, adj=0, xpd=T)
      }
      else {
        axis(2, at=seq(ylim[1],ylim[2], by=50), las=1)
        text(rep(c("female", "male"), 2), x=b, y=10, col="lightgrey", font=2, srt=90, adj=0, xpd=T)
      }
      
      if (j == 1) ylim=c(0,500) else ylim=c(0,50)
      h <- hist(soc_dat$age, breaks=20, col=pcs[3], border="white", las=1, xlab="Age", xaxt="n", main="Age", xlim=c(15, 85), ylim=ylim)
      axis(1, at=seq(15, 85, by=10))
      
      
      par(mar=c(7,4,2,1))
      
      if (j == 1) ylim=c(0,1400) else ylim=c(0,200)
      barplot(table(soc_dat$education), names=c("None", "Basic degree", "High school", "College"), border=F, col=pcs[4], las=2, ylim=ylim, main="Education", ylab="Frequency")
      # 0 Keine abgeschlossene Ausbildung
      # 1 Obligatorische Schulzeit (Primar- & Sekundarschule)
      # 2 Matura / Mittelschule
      # 3 Hochschulabschluss (Fachhochschule / Universität)
      
      if (j == 1) ylim=c(0,2000) else ylim=c(0,200)
      barplot(table(soc_dat$occupation)[c("none", "education", "house", "employed", "selfemployed", "retired")], names=c("None", "In education", "House-wife /\n-husband", "Employed", "Self-employed", "Retired"), border=F, col=pcs[5], las=2, ylim=ylim, main="Occupation", ylab="Frequency")
      # keine Haupterwerbstätigkeit; arbeitslos / auf Jobsuche
      # keine Haupterwerbstätigkeit; in Ausbildung
      # keine Haupterwerbstätigkeit; Hausfrau / Hausmann
      # angestellte Erwerbstätigkeit
      # selbständige Erwerbstätigkeit
      # in Pension / Rente
      
      dev.off()
    
    }
  }
  
  
}