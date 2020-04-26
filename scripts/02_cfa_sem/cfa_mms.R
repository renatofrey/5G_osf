# This script implements the measurement models and stores the extracted factor values

library(polycor)
library(lavaan)
library(semPlot)
library(corrplot)

load(file="data/clean/5g_data.Rdata")


sel <- "w1"
d <- get(paste("d_", sel, sep=""))
rownames(d) <- d$pid

# generate correlation matrices
d <- d[,v$mms]

# scale data
d[] <- apply(d, 2, scale)

# compare correlation matrices
c_pears <- cor(d, use="complete.obs", method = "pearson")
c_spear <- cor(d, use="complete.obs", method = "spearman")
c_diffs <- c_pears - c_spear
abs(round(summary(c_diffs[lower.tri(c_diffs)]), 3))
cormat <- c_pears

set.seed(12345)
ind_expl <- sample(1:nrow(d), size=ceiling(nrow(d)/2))
ind_conf <- setdiff(1:nrow(d), ind_expl)

p_res <- F
estimator=sel_est <- "DWLS"

set.seed(777)

# adjust some labels for plotting
names_old <- names(d)
names_new <- names(d)
names_new <- gsub("dread", "dr", names_new)
names_new <- gsub("unknown", "un", names_new)
names_new <- gsub("riskpref", "rpref", names_new)
names_new <- gsub("openness", "open", names_new)
names_new <- gsub("risk_5g", "risk", names_new)
names_new <- gsub("policy_acc", "policy_accep", names_new)
names_new <- gsub("unknown", "unkno", names_new)
names_new <- gsub("know_", "", names_new)
names_new <- gsub("bene_", "", names_new)
names_new <- gsub("policy_", "", names_new)
names(d) <- names_new
rownames(cormat) <- names_new[match(names_old, row.names(cormat))]


# define model strings
m_strings <- list(
  
prisk = '
PRISK =~ risk',
  
benefits = '
PBENE =~ pers + soc + econ',
  
dread = paste('
DREAD =~ ', paste(paste("dr", 1:5, sep=""), collapse=" + "), sep=""),

unknown = paste('
UNKNO =~ ', paste(paste("un", 1:5, sep=""), collapse=" + "), sep=""),

trust = paste('
TRUST =~ trust'),

knowledge = 
#KNOWL =~ radiat + freq + limits + cover + subj + media',
'
KNsub =~ subj + media
KNobj =~ radiat + freq + limits + cover
subj~~0*subj',

progress = '
PROGR =~ rpref + open + digit',

policy = '
POLICY =~ accep + vote + regul + resea'

)


# create groups and define colors
g <- list()
g$drivers <- c(v$psychframe, "trust", v$`5g`[grepl("know_", v$`5g`)], "rpref", "open", "digit", "DREAD", "UNKNO", "TRUST", "KNOWL", "KNobj", "KNsub", "PROGR")
g$riskben <- c("risk", "pers", "soc", "econ", "PRISK", "PBENE")
g$policy <- c(v$`5g`[grepl("policy_", v$`5g`)], "POLICY")
g <- lapply(g, function(x) {replace(x, which(is.element(x, names_old)), names_new[na.omit(match(x, names_old))])})



# plot measurement models
pdf(paste("output/", sel, "/m_measurement.pdf", sep=""), width=8, height=4)
par(mfrow=c(2,4))

fitindices <- NULL
factors <- NULL
for (i in 1:length(m_strings)) {
  m_string <- m_strings[[i]]
  
  mod <- sem(m_string, data=d, std.lv=T, estimator=sel_est)
  #summary(mod, fit.measures=T)
  pars <- parameterestimates(mod, standardize=T)
  err <- subset(pars, lhs == rhs & est!= 1)$std.all
  r2 <- 1 - (sum(err) / length(err))
  fitm <- fitmeasures(mod)
  fitindices <- rbind(fitindices,
                      c(fitm[c("npar", "df", "cfi", "tli", "bic", "srmr", "rmsea")], r2 = r2))
  
  info <- paste("CFI =", round(fitm["cfi"], 2), 
                "| TLI =", round(fitm["tli"], 2),
                "| RMSEA =", round(fitm["rmsea"], 2),
                "| R2 =", round(r2, 2), sep=" ")
  
  if (i <= 4) {ms <- c(5,4,6,4); r <- 1;}
  else {ms <- c(6,4,5,4); r <- 1}

  s <- semPaths(mod, layout="tree", rotation=r, whatLabel="std", nCharNodes=6, intercepts=F, thresholds=F, residuals=p_res, exoCov=T, title=F, sizeMan=17, sizeLat=30, mar=ms, edge.width=2, edge.label.cex=2, groups=g, color=cols, legend=F, DoNotPlot=T)
  
  s$graphAttributes$Edges$labels <- gsub("0.", ".", s$graphAttributes$Edges$labels, fixed=T)
  
  if (i == 4) s$graphAttributes$Edges$curve[7] <- 4
  plot(s)

  L <- LETTERS[i]
  if (sel == "w1" & i == 6) L <- paste(LETTERS[i], "'", sep="")
  text(-1,1.25, L, xpd=T)
  text(0, -1.4, info, xpd=T, cex=.7)
  
  
  # predict factor values
  p <- predict(mod)
  curr_vars <- colnames(inspect(mod)$theta)
  rows_ok <- which(rowSums(cbind(is.na(d[,curr_vars]))) == 0)
  p_ok <- matrix(NA, nrow=nrow(d), ncol=ncol(p))
  colnames(p_ok) <- colnames(p)
  p_ok[rows_ok,] <- p

  factors <- cbind(factors, p_ok)
  
}

fitindices <- round(fitindices, 3)

rownames(factors) <- rownames(d)
write.csv(factors, file=paste("data/clean/", sel, "_factors.csv", sep=""))

dev.off()



library(corrplot)
pdf(paste("output/", sel, "/cor_factors.pdf", sep=""))
cormat <- cor(factors, use="complete.obs")
corrplot(cormat, method="color", type="lower", diag=T, is.cor=T, mar=c(0,0,1,1), addCoef.col = "black", number.font=1, number.cex=.8, tl.col="red", cl.cex=1, cl.ratio=0.05, xpd=T)
dev.off()

cors <- cor(factors[,3:8], use="complete.obs")
summary(abs(cors[lower.tri(cors)]), 2)