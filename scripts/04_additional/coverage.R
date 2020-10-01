# This script visualizes participants' knowledge concerning 5G coverage (as a function of actual coverage as obtained from a GIS analysis) 

load(file="data/clean/5g_data.Rdata")

sel <- "w1"
d <- get(paste("d_", sel, sep=""))

pdf(file=paste("output/", sel, "/coverage.pdf", sep=""), width=5)

par(mfrow=c(3,1))

pcs <- c("black", "darkgreen", "darkred")
pts <- c("All participants", "Participants with 5G coverage", "Participants without 5G coverage")

for (i in 1:3) {

  par(mar=c(5,4,1,2))
  
  if (i == 1) dat <- d$know_cover_raw
  if (i == 2) dat <- subset(d, coverage == TRUE)$know_cover_raw
  if (i == 3) dat <- subset(d, coverage == FALSE)$know_cover_raw
  
  par(lwd=0.5)
  hist(dat, breaks=50, border="white", col=pcs[i], las=1, main="", xlab="", ylim=c(0,500), xaxt="n")
  axis(1, seq(0, 100, length.out=5))
  text(50, 500, pts[i], xpd=T, font=2, cex=1.5, xpd=T)
  
  if (i <= 3) {
    ypos <- -175
    text(0, ypos, "Certain that no\ncoverage exists", xpd=T, font=2)
    text(50, ypos, "Unsure", xpd=T, font=2)
    text(100, ypos, "Certain that \ncoverage exists", xpd=T, font=2)
  }

}

dev.off()