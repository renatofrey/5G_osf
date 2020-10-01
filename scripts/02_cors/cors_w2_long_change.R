load(file="data/clean/5g_data.Rdata")

library(viridis)
library(psych)
library(beanplot)
library(scales)

l0 <- function(x) sub("^(-?)0.", "\\1.", sprintf("%.2f", x))


pdf(file="output/w2_long_change/cors_prisk.pdf", width=6, height=4)

ivs_all <-c("Dread" = "sc_dread",
            "Unknown" = "sc_unkno",
            "Subj. knowledge" = "sc_kns",
            "Obj. knowledge" = "sc_kno",
            "Trust" = "trust",
            "Progression" = "sc_progr",
            "Gender" = "sex",
            "Occupation" = "occupation",
            "Education" = "education",
            "Pol. Att." = "polatt",
            "age" = "age",
            "EHS" = "ehs")

ivs <-c("Dread" = "sc_dread",
        "Unkn.risk" = "sc_unkno",
        "Subj.knowl." = "sc_kns",
        "Obj.knowl." = "sc_kno",
        "Trust" = "trust"
)


# ivs <-c("Dread" = "sc_dread",
#         "Unknown" = "sc_unkno",
#         "Subj. knowledge" = "sc_kns",
#         "Obj. knowledge" = "sc_kno",
#         "Trust" = "trust",
#         "Progression" = "sc_progr",
#         "Pol. Att." = "polatt",
#         "EHS" = "ehs")

n1 <- 4
n2 <- 4

m <- rbind(matrix(rep(1:length(ivs), n1), nrow=n1, byrow=T),
           matrix(rep(length(ivs) + 1:length(ivs), n2), nrow=n2, byrow=T)
                  )
m <- m[,rep(1:ncol(m), each=3)]

m <- cbind(c(rep(max(m)+2, n1), rep(max(m)+1, n2)),
           c(rep(max(m)+2, n1), rep(max(m)+1, n2)),
           m)
layout(m)

x.lim <- c(-50,50)
y.lim <- c(-50,50)


mx <- c(0,.3,2,.7)
par(mar=mx, mgp=c(2,1,0))

pcs <- c("blue", "red", "green", "darkgrey")
pcs <- c(inferno(3, begin=.4, end=.9), "darkgrey")


cp <- function(vx, vy, curr_cond) {
  
  curr_d <- subset(d_w2_long_change, cond == curr_cond)
  #curr_d <- d_w1
  x <- curr_d[,vx]
  y <- curr_d[,vy]
  r <- round(cor(x, y, use="complete.obs"), 2)
  
  if (curr_cond == "A_mgmtsum") pc = pcs[1]
  if (curr_cond == "B_pressrel") pc = pcs[2]
  if (curr_cond == "C_keypoints") pc = pcs[3]
  if (curr_cond == "D_control") pc = pcs[4]
  
  if (curr_cond == "A_mgmtsum") {
    ti <- names(which(ivs == iv))
    pt <- bquote(expression(bold(Delta~.(ti))))

    plot(x, y, las=1, xlab="", ylab="", las=1, main=eval(pt), cex=.6, xlim=x.lim, ylim=y.lim, col=pc, xaxt="n", yaxt="n", frame=F, pch=16, cex.main=1.45)
    box(lwd=.1)
    if (iv == head(ivs, 1)) {
      axis(2, at=seq(y.lim[1], y.lim[2], length.out=5), las=1, mgp=c(2.5,1.5,1), cex.axis=1) 
    }
    
  }
  else points(x, y, cex=.6, col=pc, pch=16)
  m <- lm(y ~ x)
  abline(m, col=pc, lwd=3)
  print(r)
  
  
  if (curr_cond == "D_control") {
    all_x <- d_w2_long_change[,vx]
    all_y <- d_w2_long_change[,vy]
    all_r <- cor(all_x, all_y, use="complete.obs")
    
    lmdat <- data.frame(all_x,all_y)
    all_m <- lm(all_y ~ all_x, dat=lmdat)
    
    xr <- -10:110
    preds <- predict(all_m, newdata=data.frame(all_x=xr), interval="confidence")
    ccol <- alpha("tan3", alpha=.5)
    polygon(c(rev(xr), xr), c(rev(preds[ ,3]), preds[ ,2]), col=ccol, border = NA)
  
    abline(all_m, lwd=4, col="black", lty=1, lend=1)
    lcol <- alpha("white", alpha=.8)
    par(font=2)
    legend("topright", paste("r = ", l0(round(all_r,2)), sep=""), bg=lcol, box.lty=0, inset=0.01, y.intersp=0,x.intersp=0, cex=1.2)
    par(font=1)
    
  }
  
}
  






for(iv in ivs) {
  for (curr_cond in levels(d_w2_long_change$cond)) {
    cp(iv, "risk_5g", curr_cond)
  }
}


pcs2 <- sapply(rev(pcs), function(x) {list(c(x, rep("black", 3)))})

for(iv in ivs_all) {
  
  diffs <- data.frame(pid=d_w2_long$pid,
                      cond=d_w2_long$cond,
                      t1=d_w1[match(d_w2_long$pid, d_w1$pid), iv],
                      t2=d_w2_long[, iv])
  if (is.factor(diffs$t1)) {
    diffs$t1 <- droplevels(diffs$t1)
    diffs$t2 <- droplevels(diffs$t2)
  }
  diffs$ident <- diffs$t1 == diffs$t2
  sgroups <- by(diffs, diffs$cond, list)
  
  # get kappa or ICCs for control group
  diffs_ctrl <- subset(diffs, cond == "D_control")
  if (is.factor(diffs_ctrl[,"t1"])) {
    kappa <- round(cohen.kappa(diffs_ctrl[,c("t1","t2")])$kappa, 2)
    print(paste(iv, kappa, sep=":"))
  } else {
    ICC <- round(ICC(diffs_ctrl[,c("t1","t2")])$results[1,"ICC"], 2)
    print(paste(iv, ICC, sep=":"))
  }
  
  if (!is.element(iv, ivs)) next

  iccs <- lapply(sgroups, function(x) {
    ICC(x[,c("t1", "t2")])$results[1,"ICC"]
  })
  cors <- lapply(sgroups, function(x) {
    cor(x[,c("t1", "t2")])[1,2]
  })
  
  # iccs <- by(diffs, diffs$cond, function(x) {
  #   ICC(x[,c("t1", "t2")])$results[1,"ICC"]
  # })
  print(iv)
  print(cbind(iccs,cors))
  
  if (!is.element(iv, ivs)) next

  
  par(mar=c(2,.3,0,.7))
  dd <- rev(tapply(d_w2_long_change[,iv], d_w2_long_change$cond, list))
  dd_means <- lapply(dd, median, na.rm=T)
  #dd <- d_w1[,iv]
  plot(1, type="n", xlim=x.lim, ylim=c(1,3), frame=F, las=1, xaxt="n", yaxt="n", xlab="", ylab="")
  
    
  yb <- seq(1,2.5, length.out=4)
  
  if (!is.element(iv, c("age", "education"))) {
    b1 <- beanplot(at=yb, dd, horizontal=T, las=1, what=c(0,1,0,0), side="second", col=pcs2, border=F, frame.plot=F, cutmin=-50, cutmax=50, add=T, yaxt="n", beanlinewd=1.5, bw=5)
  }
  lines(c(0,1.5), c(0,3), lty=1, lwd=1, col="white")
  #text(x=rep(35,4), y=yb+.5, paste("ICC=", l0(round(rev(unlist(iccs)), 2)), sep=""), cex=1.1, pos=1, xpd=T)
  text(x=50, y=max(yb)+.45, substitute("ICCs:"), xpd=T, adj=1, cex=1.1)
  text(x=rep(50,4), y=yb+.23, paste(l0(round(rev(unlist(iccs)), 2)), sep=""), cex=1.1, adj=1, xpd=T)
  
  ll <- .4
  for (i in 1:length(dd_means)) {
    lines(x=c(dd_means[i], dd_means[i]), y=c(yb[i], yb[i]+ll), col="black", lwd=1.5, lend=1, lty=1)
  } 
  
  #abline(v=0, col="white")
  
  #abline(v=lapply(dd, mean), col="white")
  
  axis(1, at=seq(x.lim[1], x.lim[2], length.out=5), las=1, mgp=c(0,.5,0), cex.axis=1, c(-50, "", 0, "", 50)) 
  
  abline(v=-100)
  abline(v=100)
}

par(mar=c(2,0,0,0))
plot(1, type="n", xlim=x.lim, ylim=c(1,3), frame=F, las=1, xaxt="n", yaxt="n", xlab="", ylab="")

#text("Experimental conditions", font=2, x=-50, y=.8, pos=4, srt=90, xpd=T)

text(c("Control\ngroup", "Key points", "Press\nrelease", "Summary"), x=-60, y=seq(1.,2.5, length.out=4)+.08, pos=4, cex=1.3, xpd=T)
text("Condition:", x=-60, y=2.85, pos=4, cex=1.2, xpd=T, font=2)

#text(c("No informa-\ntion (control)", "Key points of\nexpert report", "Press release\nexpert report", "Summary of\nexpert report"), x=-60, y=seq(1.,2.5, length.out=4)+.12, pos=4, cex=1, xpd=T)

#text(c("D:", "C:", "B:", "A:"), x=-60, y=seq(1.,2.5, length.out=4)+.25, pos=4, cex=.9, xpd=T)


frame()
text(expression(bold(paste(Delta, " Risk perception"))), srt=90, x=0.2,y=.38, pos=1, cex=1.5, xpd=T, font=3)

dev.off()