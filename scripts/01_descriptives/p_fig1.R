load(file="data/clean/5g_data.Rdata")

library(viridis)
library(beanplot)
library(scales)

l0 <- function(x) sub("^(-?)0.", "\\1.", sprintf("%.2f", x))

pdf(file="output/w1/fig1.pdf", width=10, height=2.5)


ivs <-c("5G-Dread" = "sc_dread",
        "EHS" = "ehs",
        "5G-Unknown risk" = "sc_unkno",
        "Age" = "age",
        "5G-Knowl (subj.)" = "sc_kns",
        "Political att." = "polatt",
        "Education" = "education",
        "Risk preference" = "riskpref",
        "Openness" = "openness",
        "5G-Knowl. (obj.)" = "sc_kno",
        "Progression" = "sc_progr",
        "Digitalization" = "digit",
        "5G-Trust" = "trust"
)

ivs <-c("Dread" = "sc_dread",
        "Unknown risk" = "sc_unkno",
        "Subj. knowl." = "sc_kns",
        "Obj. knowl." = "sc_kno",
        "Trust" = "trust",
        "EHS" = "ehs",
        "Age" = "age",
        "Political att." = "polatt",
        #"Education" = "education",
        "Progression" = "sc_progr"
)

n1 <- 4
n2 <- 2

m <- rbind(
           matrix(rep(length(ivs) + 1:length(ivs), n2), nrow=n2, byrow=T),
           matrix(rep(1:length(ivs), n1), nrow=n1, byrow=T)
                  )

m <- m[,rep(1:ncol(m), each=2)]

m <- cbind(c(rep(max(m)+1, n2),
             rep(max(m)+2, n1)), m)
layout(m)


mx <- c(1.75,.2,0,.4)
par(mar=mx, mgp=c(2,1,0))


cp <- function(vx, vy) {
  
  curr_d <- d_w1
  x <- curr_d[,vx]
  y <- curr_d[,vy]
  r <- round(cor(x, y, use="complete.obs"), 2)
  
  plot(x, y, las=1, xlab="", ylab="", las=1, cex=.4, xlim=x.lim, ylim=y.lim, col=pc, xaxt="n", yaxt="n", frame=F, pch=16, font.main=1, cex.main=1.4)
  box(lwd=.1)
  if (iv == head(ivs, 1)) {
    axis(2, at=seq(y.lim[1], y.lim[2], length.out=5), las=1, mgp=c(1,.75,.5), cex.axis=1) 
  }
  
  if (iv != "age") axis(1, at=seq(x.lim[1], x.lim[2], length.out=5), las=1, mgp=c(0,.5,0.2), cex.axis=.9)
  else axis(1, at=round(seq(x.lim[1], x.lim[2], length.out=5), 0), las=1, mgp=c(0,.5,0.2), cex.axis=.9) 
  
  
  r <- cor(x, y, use="complete.obs")
  lmdat <- data.frame(x,y)
  m <- lm(y ~ x, dat=lmdat)

  xr <- -10:110
  preds <- predict(m, newdata=data.frame(x=xr), interval="confidence")
  ccol <- scales::alpha("darkgrey", alpha=.7)
  polygon(c(rev(xr), xr), c(rev(preds[ ,3]), preds[ ,2]), col=ccol, border = NA)
  abline(m, lwd=2, col="black", lty=1, lend=1, xpd=F)
  
  lcol <- scales::alpha("white", alpha=.8)
  par(font=2)
  legend("bottomright", paste("r =", l0(round(r,2))), bg=lcol, box.lty=0, inset=0, x.intersp=-.5, y.intersp=-.05, cex=1.2)
  par(font=1)
}
  




for (iv in ivs) {
  
  pc <- viridis(1, begin=.3)
  if (is.element(iv, tail(ivs, 4))) pc <- viridis(1, begin=.6)
  
  x.lim <- c(0,100)
  y.lim <- c(0,100)
  
  if (iv == "education") {
    x.lim <- c(-1,4)
    y.lim <- c(0,100)  
  }
  
  if (iv == "age") {
    x.lim <- c(18,80)
    y.lim <- c(0,100)  
  }
  
    cp(iv, "risk_5g")
  
}


for(iv in ivs) {
  
  x.lim <- c(0,100)
  y.lim <- c(0,100)
  
  if (iv == "education") {
    x.lim <- c(-1,4)
    y.lim <- c(0,100)  
  }
  
  if (iv == "age") {
    x.lim <- c(18,80)
    y.lim <- c(0,100)  
  }
  
  pc <- viridis(1, begin=.3)
  if (is.element(iv, tail(ivs, 4))) pc <- viridis(1, begin=.6)
  pcs2 <- sapply(pc, function(x) {list(c(x, rep("black", 3)))})
  
  
  ti <- names(which(ivs == iv))
  pt <- bquote(paste("", .(ti)))
  pt <- ti
  
  
  
  par(mar=c(0,.2,2,.4))
  dd <- d_w1[,iv]
  dd_means <- mean(dd, na.rm=T)

  plot(1, type="n", xlim=x.lim, ylim=c(1,1.5), frame=F, las=1, xaxt="n", yaxt="n", xlab="", ylab="", main=pt, cex.main=1.4)
  
  yb <- seq(1,2.5, length.out=4)
  b1 <- beanplot(at=1, dd, horizontal=T, las=1, what=c(0,1,0,0), side="second", col=pcs2, border=F, frame.plot=F, cutmin=x.lim[1], cutmax=x.lim[2], add=T, yaxt="n", beanlinewd=1.5, bw=5)
  if (iv != "age") lines(x=c(50,50), y=c(01,1.4), lty=1, lwd=1, col="white", xpd=F)
  xt <- 50
  if (iv == "age") xt <- 48
  curr_mean <- round(mean(dd,na.rm=T),0)
  text(xt,1.5, paste("M =", curr_mean), adj=0.5, xpd=T, cex=1.1)

  ll <- .4
  for (i in 1:length(dd_means)) {
    lines(x=c(dd_means[i], dd_means[i]), y=c(yb[i], yb[i]+ll), col="black", lwd=1.5, lend=1, lty=1)
  } 
  
}

plot(1, type="n", xlim=x.lim, ylim=c(1,3), frame=F, las=1, xaxt="n", yaxt="n", xlab="", ylab="")

frame()
text(expression(bold("Perceived risk")), srt=90, x=0,y=.7, pos=1, cex=1.4, xpd=T, font=2)

dev.off()