# This script visualizes information related to the interventions in the field experiment

library(beanplot)

load(file="data/clean/5g_data.Rdata")

for (j in 1:3) {
  
  sel <- "w2_long"
  d_sel <- get(paste("d_", sel, sep=""))
  
  # Run analyses also separately for the two extreme groups
  if (j == 1) {
    add <- ""
  }
  if (j == 2) {
    d_sel <- subset(d_sel, ext_grp == "low")
    add <- "_low"
  }
  if (j == 3) {
    d_sel <- subset(d_sel, ext_grp == "high")
    add <- "_high"
  }
  
  levels(d_sel$cond) <- gsub("A_mgmtsum", "Group A:\nSummary", levels(d_sel$cond))
  levels(d_sel$cond) <- gsub("B_pressrel", "Group B:\nPress release", levels(d_sel$cond))
  levels(d_sel$cond) <- gsub("C_keypoints", "Group C:\nBullet points", levels(d_sel$cond))
  levels(d_sel$cond) <- gsub("D_control", "Group D:\nControl", levels(d_sel$cond))
  
  sel_vs <- names(d_sel)[grepl("rep_", names(d_sel))]
  
  pdf(paste("output/", sel, "/expreport", add, ".pdf", sep=""), height=5, width=11)
  par(mfrow=c(1,4))
  
  for (sel_v in sel_vs) {
    
    if (is.factor(d_sel[,sel_v])) d_sel[,sel_v] <- droplevels(d_sel[,sel_v])
    dat <- tapply(d_sel[,sel_v], list(d_sel$cond), list)
    dat <- rev(dat)
    
    if (sel_v == "rep_aware") {
      x <- lapply(dat, table)
      m <- matrix(unlist(x), ncol=2, byrow=T)
      rownames(m) <- names(x)
      
      cs <- c("coral2", "palegreen3")
      par(mar=c(4,8,2,2), mgp=c(2,1,0))
      if (j == 1) xlim <- c(0, 250) else xlim <- c(0, 40)
      
      b <- barplot(t(m), beside=F, horiz=T, las=1, xlim=xlim, col=cs, border=0, xaxt="n", xlab="Number of participants", cex.lab=1.2, space=1.5, ylim=c(.75,10.75), cex.names=1.2, main="Aware of ER?")
      if (j == 1) axis(1, at=seq(0, 250, by=50), cex.axis=1, mgp=c(3,.5,0)) else axis(1, at=seq(0, 50, by=10), cex.axis=1, mgp=c(3,.5,0))
      
      legend("top", pch=15, c("No", "Yes"), col=cs, box.lty=0, horiz=T)
    }
    else {
      
      par(mar=c(4,1,2,2))
      
      plot(1, xlim=c(0,100), ylim=c(0.5,4.5), type="n", frame=F, xlab="", ylab="", xaxt="n", yaxt="n")
      
      abline(v=50, lty=3)
      
      p_bw <- 10
      pc <- "bisque3"
      if (sel_v == "rep_engaged") p_main <- "How much engaged with ER?"
      if (sel_v == "rep_informed") p_main <- "How well informed by ER?"
      if (sel_v == "rep_changed") p_main <- "How much did ER influence opinion?"
      
      b1 <- beanplot(dat, bw=p_bw, what=c(0,1,0,1), method="jitter", jitter=.1, boxwex=1, ll=.05, beanlinewd=0.5, border=0, xlab="", ylim=c(0,100), col=pc, las=1, cex.axis=.8, cex.lab=.8, horizontal=T, xaxt="n", yaxt="n", type="n", add=T, cutmin=0, cutmax=100, main=p_main)
      
      axis(1, at=seq(0, 100, length.out=5), cex.axis=1, mgp=c(3,.5,0))
      axis(1, c("Not all", "Neither", "A lot"), at=seq(0, 100, length.out=3), mgp=c(3,2,0), cex.axis=1.2)
      
      mlength <- .1
      for (l in 1:4) {
        lines(rep(mean(dat[[l]], na.rm=T), 2), c(l-mlength, l+mlength), col="red", lwd=2)
      }
      
      #axis(2, at=1:4, names(dat), las=1, mgp=c(1,.2,0), tick=F, hadj=0.5, pos=-15, cex.axis=1.1)
      
      
    }
    
  }
  
  dev.off()
  
}