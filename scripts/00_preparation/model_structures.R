# This script produces the structures of the measurement models for the preregistration and plots the predicted associations.
# => The script generates some arbitrary data to be able to plot the models nicely; warnings can thus be ignored.

library(lavaan)
library(semPlot)
library(viridis)

vcols <- viridis(3, begin = .80, end = .2, alpha=.6)
barplot(1:3, col=vcols)
vcols <- c(vcols, "white")

v <- list("drivers" = c(paste("dr", 1:5, sep=""),
                        paste("un", 1:5, sep=""),
                        "trust",
                        "subj", "media", "obj1", "obj2", "obj3",
                        "rpref", "open", "digit"),
          "riskben" = c("risk",
                        "pers", "soc", "econ"),
          "policy" = c("regul", "resea", "vote", "accep"),
          "other" = c("sex", "age", "edu", "ehs", "polit"))
vs <- as.character(unlist(v))
d <- as.data.frame(matrix(rnorm(length(v) * 3000), ncol=length(vs)))
colnames(d) <- vs

g <- v
g$drivers <- c(g$drivers, "DREAD", "UNKNO", "TRUST", "KNOW", "PROGR")
g$riskben <- c(g$riskben, "PRISK", "PBENE")
g$policy <- c(g$policy, "POLICY")



m1 <- paste('DREAD =~ ', paste(paste("dr", 1:5, sep=""), collapse=" + "), ' ', sep="")
m2 <- paste('UNKNO =~ ', paste(paste("un", 1:5, sep=""), collapse=" + "), ' ', sep="")
m3 <- 'TRUST =~ trust'
m4 <- 'KNOW =~ subj + media + obj1 + obj2 + obj3'
m5 <- 'PROGR =~ rpref + open + digit'
m6 <- 'PRISK =~ risk'
m7 <- 'PBENE =~ pers + soc + econ'
m8 <- 'POLICY =~ regul + resea + vote + accep'

pdf("output/prereg/cfa_meas.pdf", width=7, height=3)
par(mfrow=c(2,4))

for (i in 1:8) {
  
  str <- get(paste("m", i, sep=""))
  
  if (i <= 4) {ms <- c(5,4,6,4); r <- 1;}
  else {ms <- c(6,4,5,4); r <- 1}
  
  cols <- "white" #c("lightgrey", "white", "white")
  if (i <= 5) cols[1] <- vcols[1]
  if (is.element(i, 6:7)) cols[1] <- vcols[2]
  if (i == 8) cols[1] <- vcols[3]
  
  
  #if (is.element(i, c(5,8))) frame()
   {
    m <- sem(str, data=d, std.lv=T)  
    semPaths(m, layout="tree", rotation=r, whatLabel="none", nCharNodes=6, intercepts=T, thresholds=F, residuals=F, exoCov=T, title=F, sizeMan=20, sizeLat=35, mar=ms, edge.width=2, color=cols)
    text(-1,1.25, LETTERS[i], xpd=T)
  }
}  

dev.off()





pdf("output/prereg/cfa_sem1.pdf", width=7, height=2)

s1 <- 10
s2 <- 20
ms <- c(2,4,2,4)

par(mfrow=c(1,3))

str <- paste(m1, '\n', m2, '\n', m6, '\n', m7, '
PRISK ~ DREAD + UNKNO
PBENE ~ DREAD + UNKNO',
             sep="")
cat(str)
m <- sem(str, data=d, std.lv=T)  
s <- semPaths(m, layout="tree", rotation=1, whatLabel="none", nCharNodes=6, intercepts=T, thresholds=F, residuals=F, exoCov=T, title=F, sizeMan=s1, sizeLat=s2, mar=ms, edge.width=2, groups=g, color=vcols, legend=F, DoNotPlot=T)
l <- s$graphAttributes$Edges$labels
n <- length(s$Edgelist$from)
s$graphAttributes$Edges$edge.label.position[16] <- .25
s$graphAttributes$Edges$edge.label.position[17] <- .25

s$graphAttributes$Edges$labels[15] <- "+"
s$graphAttributes$Edges$labels[16] <- "+"
s$graphAttributes$Edges$labels[17] <- "-"
s$graphAttributes$Edges$labels[18] <- "-"
s$graphAttributes$Edges$labels[20] <- "-"
s$graphAttributes$Edges$label.cex <- rep(4, n)
plot(s)
text(-1.3,1.1, "A", xpd=T)




str <- paste(m3, '\n', m4, '\n', m6, '\n', m7, '
PRISK ~ TRUST + KNOW
PBENE ~ TRUST + KNOW',
             sep="")
cat(str)
m <- sem(str, data=d, std.lv=T)  
s <- semPaths(m, layout="tree", rotation=1, whatLabel="none", nCharNodes=6, intercepts=T, thresholds=F, residuals=F, exoCov=T, title=F, sizeMan=s1, sizeLat=s2, mar=ms, edge.width=2, groups=g, color=vcols, legend=F, DoNotPlot=T)
l <- s$graphAttributes$Edges$labels
n <- length(s$Edgelist$from)
s$graphAttributes$Edges$edge.label.position[12] <- .25
s$graphAttributes$Edges$edge.label.position[13] <- .25

#s$graphAttributes$Edges$labels[1:n] <- as.character(1:n)
s$graphAttributes$Edges$labels[11] <- "-"
s$graphAttributes$Edges$labels[12] <- "-"
s$graphAttributes$Edges$labels[13] <- "+"
s$graphAttributes$Edges$labels[14] <- ""
s$graphAttributes$Edges$labels[16] <- "-"
s$graphAttributes$Edges$label.cex <- rep(4, n)
plot(s)
text(-1.3,1.1, "B", xpd=T)




str <- paste(m5, '\n', m6, '\n', m7, '
PRISK ~ PROGR
PBENE ~ PROGR',
             sep="")
cat(str)
m <- sem(str, data=d, std.lv=T)  
s <- semPaths(m, layout="tree", rotation=1, whatLabel="none", nCharNodes=6, intercepts=T, thresholds=F, residuals=F, exoCov=T, title=F, sizeMan=s1, sizeLat=s2, mar=ms, edge.width=2, groups=g, color=vcols, legend=F, DoNotPlot=T)
l <- s$graphAttributes$Edges$labels
n <- length(s$Edgelist$from)
# s$graphAttributes$Edges$edge.label.position[16] <- .25
# s$graphAttributes$Edges$edge.label.position[17] <- .25

#s$graphAttributes$Edges$labels[1:n] <- as.character(1:n)
s$graphAttributes$Edges$labels[8] <- "-"
s$graphAttributes$Edges$labels[9] <- "+"
s$graphAttributes$Edges$labels[10] <- "-"
s$graphAttributes$Edges$label.cex <- rep(4, n)
plot(s)
text(-1.3,1.1, "C", xpd=T)



dev.off()







pdf("output/prereg/cfa_sem2.pdf", width=2.5, height=2)

s1 <- 5
s2 <- 10
ms <- c(2,6,2,6)

#par(mfrow=c(1,3))
#frame()

str <- paste(m6, '\n', m7, '\n', m8, '\n
POLICY ~ PRISK + PBENE',
             sep="")
cat(str)
m <- sem(str, data=d, std.lv=T)  
s <- semPaths(m, layout="tree", rotation=1, whatLabel="none", nCharNodes=6, intercepts=T, thresholds=F, residuals=F, exoCov=T, title=F, sizeMan=s1, sizeLat=s2, mar=ms, edge.width=1, groups=g, color=vcols, legend=F, DoNotPlot=T)
l <- s$graphAttributes$Edges$labels
n <- length(s$Edgelist$from)
# s$graphAttributes$Edges$edge.label.position[16] <- .25
# s$graphAttributes$Edges$edge.label.position[17] <- .25
# 
# s$graphAttributes$Edges$labels[1:n] <- as.character(1:n)
s$graphAttributes$Edges$labels[9] <- "-"
s$graphAttributes$Edges$labels[10] <- "+"
# s$graphAttributes$Edges$labels[17] <- "-"
# s$graphAttributes$Edges$labels[18] <- "-"
# s$graphAttributes$Edges$labels[20] <- "-"
s$graphAttributes$Edges$label.cex <- rep(1.5, n)
plot(s)
# text(-1.3,1.1, "A", xpd=T)

dev.off()