# This script implements the SEMs with PRISK and PBENE as DVs and runs the respective Bayesian model selection
# => sample needs to be selected ('sel') in cfa_mms.R

# load measurement models
source("scripts/02_cfa_sem/cfa_mms.R")

# SEMs
p_res <- F

pdf(file=paste("output/", sel, "/m_sem.pdf", sep=""), width=8, height=3)

par(mfrow=c(1,4))

size1 <- 9
size2 <- 18
ms <- c(4,3,4,3)

str1 <- paste(
  m_strings$dread,
  m_strings$unknown,
  '
PRISK ~ DREAD + UNKNO
PBENE ~ DREAD + UNKNO
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str1)
m1 <- sem(str1, data=d, std.lv=T, estimator=sel_est)
fact1 <- predict(m1)

s1 <- semPaths(m1, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s1$graphAttributes$Edges$labels <- gsub("0.", ".", s1$graphAttributes$Edges$labels, fixed=T)
s1$graphAttributes$Edges$labels <- gsub("1.00", "1", s1$graphAttributes$Edges$labels, fixed=T)

s1$graphAttributes$Edges$edge.label.position[1:5] <- .7
s1$graphAttributes$Edges$edge.label.position[6:10] <- .7
s1$graphAttributes$Edges$edge.label.position[12:13] <- .15

ind <- 11:14
s1$graphAttributes$Edges$color[ind] <- "black"
s1$graphAttributes$Edges$label.font[ind] <- 2

s1$graphAttributes$Edges$label.cex <- rep(1.7, length(s1$graphAttributes$Edges$labels))
s1$graphAttributes$Edges$label.cex[ind] <- 2.2

s1$graphAttributes$Nodes$label.cex <- 1.2

plot(s1)
fitm <- fitmeasures(m1)
info <- paste("CFI =", round(fitm["cfi"], 2), 
              "| TLI =", round(fitm["tli"], 2),
              "| RMSEA =", round(fitm["rmsea"], 2), sep=" ")
if (!is.na(fitm['bic'])) info <- paste(info, "| BIC =", round(fitm["bic"], 0))
text(0, -1.275, info, xpd=T, cex=.9)
text(-1.1,1.25, "A", xpd=T, cex=1.7)

str2 <- paste(
  m_strings$trust,
  m_strings$knowledge, '
PRISK ~ TRUST + KNobj + KNsub
PBENE ~ TRUST + KNobj + KNsub
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str2)
m2 <- sem(str2, data=d, std.lv=T, estimator=sel_est)
fact2 <- predict(m2)

s2 <- semPaths(m2, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s2$graphAttributes$Edges$labels <- gsub("0.", ".", s2$graphAttributes$Edges$labels, fixed=T)
s2$graphAttributes$Edges$labels <- gsub("1.00", "1", s2$graphAttributes$Edges$labels, fixed=T)

s2$graphAttributes$Edges$edge.label.position[1:7] <- .7
s2$graphAttributes$Edges$edge.label.position[c(8,9,10)] <- .15

ind <- 8:13
s2$graphAttributes$Edges$color[ind] <- "black"
s2$graphAttributes$Edges$label.font[ind] <- 2

s2$graphAttributes$Edges$curve[c(18,20)] <- 1.9
s2$graphAttributes$Edges$curve[19] <- 3

s2$graphAttributes$Edges$label.cex <- rep(1.7, length(s2$graphAttributes$Edges$labels))
s2$graphAttributes$Edges$label.cex[ind] <- 2.2

s2$graphAttributes$Nodes$label.cex <- 1.2

plot(s2)
fitm <- fitmeasures(m2)
info <- paste("CFI =", round(fitm["cfi"], 2), 
              "| TLI =", round(fitm["tli"], 2),
              "| RMSEA =", round(fitm["rmsea"], 2), sep=" ")
if (!is.na(fitm['bic'])) info <- paste(info, "| BIC =", round(fitm["bic"], 0))
text(0, -1.275, info, xpd=T, cex=.9)
text(-1.1,1.25, "B", xpd=T, cex=1.7)


str3 <- paste(
  m_strings$progress, '
PRISK ~ PROGR
PBENE ~ PROGR
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str3)
m3 <- sem(str3, data=d, std.lv=T, estimator=sel_est)
fact3 <- predict(m3)

s3 <- semPaths(m3, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s3$graphAttributes$Edges$labels <- gsub("0.", ".", s3$graphAttributes$Edges$labels, fixed=T)
s3$graphAttributes$Edges$labels <- gsub("1.00", "1", s3$graphAttributes$Edges$labels, fixed=T)

s3$graphAttributes$Edges$edge.label.position[1:3] <- .7

ind <- c(4,5)
s3$graphAttributes$Edges$color[ind] <- "black"
s3$graphAttributes$Edges$label.font[ind] <- 2

s3$graphAttributes$Edges$label.cex <- rep(1.7, length(s3$graphAttributes$Edges$labels))
s3$graphAttributes$Edges$label.cex[ind] <- 2.2

s3$graphAttributes$Nodes$label.cex <- 1.2

plot(s3)
fitm <- fitmeasures(m3)
info <- paste("CFI =", round(fitm["cfi"], 2), 
              "| TLI =", round(fitm["tli"], 2),
              "| RMSEA =", round(fitm["rmsea"], 2), sep=" ")
if (!is.na(fitm['bic'])) info <- paste(info, "| BIC =", round(fitm["bic"], 0))
text(0, -1.275, info, xpd=T, cex=.9)
text(-1.1,1.25, "C", xpd=T, cex=1.7)



# panel 4
str1 <- paste(
  m_strings$policy,
  '
POLICY ~ PRISK + PBENE
PRISK =~ 1*risk
PBENE =~ pers + soc + econ
', sep="")

#cat(str1)
m1 <- sem(str1, data=d, std.lv=T, estimator=sel_est)
fact1 <- predict(m1)

s1 <- semPaths(m1, layout="tree", rotation=1, whatLabel="std", nCharNodes=6, intercepts=T, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=size1, sizeLat=size2, mar=ms, edge.width=2, edge.label.cex=1.7, groups=g, color=cols, legend=F, DoNotPlot=T)

s1$graphAttributes$Edges$labels <- gsub("0.", ".", s1$graphAttributes$Edges$labels, fixed=T)
s1$graphAttributes$Edges$labels <- gsub("1.00", "1", s1$graphAttributes$Edges$labels, fixed=T)

s1$graphAttributes$Edges$curve[11] <- 2

ind <- 5:6
s1$graphAttributes$Edges$color[ind] <- "black"
s1$graphAttributes$Edges$label.font[ind] <- 2

s1$graphAttributes$Edges$label.cex <- rep(1.7, length(s1$graphAttributes$Edges$labels))
s1$graphAttributes$Edges$label.cex[ind] <- 2.2

s1$graphAttributes$Nodes$label.cex <- 1.2

plot(s1)
fitm <- fitmeasures(m1)
info <- paste("CFI =", round(fitm["cfi"], 2), 
              "| TLI =", round(fitm["tli"], 2),
              "| RMSEA =", round(fitm["rmsea"], 2), sep=" ")
if (!is.na(fitm['bic'])) info <- paste(info, "| BIC =", round(fitm["bic"], 0))
text(0, -1.275, info, xpd=T, cex=.9)
text(-1.1,1.25, "D", xpd=T, cex=1.7)





 
dev.off()
