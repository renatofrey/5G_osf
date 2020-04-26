# Print latex table with sociodemographic information

library(xtable)

load(file="data/clean/5g_data.Rdata")

vars <- c("sex", "age", "education", "occupation", "smartphone", "device")

names(d_w2_long) <- gsub("device_w2", "device", names(d_w2_long))


tab <- rbind(c("", "", "Study 1", "Study 2", "Study 2"),
             c("", "", "", "cross-sectional", "longitudinal"),
             c("", "", "", "", ""),
             c("N", "", as.character(nrow(d_w1)), as.character(nrow(d_w2_cross)), as.character(nrow(d_w2_long)))
)

for (var in c(vars)) {
  
  for (s in 1:3) {
    print(paste(s, var))
    
    if (s == 1) vals <- d_w1[,var]
    if (s == 2) vals <- d_w2_cross[,var]
    if (s == 3) vals <- d_w2_long[,var]
    if (s == 3 & var == "age") vals <- d_w2_long[,"age_w2"]
    if (s == 3 & var == "education") vals <- d_w2_long[,"education_w2"]
    
    if (var == "education") vals <- as.factor(vals)
    
    if (is.numeric(vals)) {
      
      out <- paste(round(mean(vals, na.rm=T), 2),
                   " (SD=", round(sd(vals, na.rm=T), 2), ")",
                   sep="")
      out <- cbind(out)
      if (s == 1) curr_out <- cbind("", out) else curr_out <- cbind(curr_out, out)
    }
    
    if (is.factor(vals)) {
      
      if (sum(vals == "") > 0) vals <- vals[-which(vals == "")]
      vals <- droplevels(vals)
      
      if (var == "education") {
        levels(vals)[which(levels(vals) == "0")] <- "none"
        levels(vals)[which(levels(vals) == "1")] <- "basic education"
        levels(vals)[which(levels(vals) == "2")] <- "high school"
        levels(vals)[which(levels(vals) == "3")] <- "college degree"
      }
      
      if (var == "occupation") {
        levels(vals)[which(levels(vals) == "education")] <- "in education"
        levels(vals)[which(levels(vals) == "house")] <- "house-wm./man"
        levels(vals)[which(levels(vals) == "selfemployed")] <- "self-employed"
      }
      
      
      ls <- unique(c(levels(d_w1[,var]), levels(d_w2_cross[,var]), levels(d_w2_long[,var])))
      
      out <- as.data.frame(table(vals))
      if (sum(!is.element(ls, out$vals)) < 0) out <- rbind(out, data.frame(vals=ls[!is.element(ls, out$vals)], Freq=0))
      out_perc <- paste(round(prop.table(out[,2])*100, 1), "%", sep="")
      out[,2] <- paste(out[,2], " / ", out_perc, sep="")
  
      if (var == "occupation") out <- out[c(1,2,6,3,5,4),]
      if (var == "smartphone") out <- out[c(3,4,1,2),]
      if (var == "education") out <- out[c(2,3,4,1),]

      
          
      if (s == 1) curr_out <- out else curr_out <- cbind(curr_out, out[match(rownames(curr_out), rownames(out)),2])
    }
    
    
  }
  
  
  curr_out <- cbind(var, curr_out)
  if (nrow(curr_out) > 1) {
    levels(curr_out$var) <- c(levels(curr_out$var), "")
    curr_out$var[2:nrow(curr_out)] <- ""
  }
  
  tab <- rbind(tab, as.matrix(curr_out, colnames=NULL))
  
  if (is.element(var, c("sex", "age", "ses_fath", "birthrank", "edu", "income", "NUM"))) tab <- rbind(tab, rep("", ncol(tab)))
  
}



labels_new <- tab[,1]
labels_new <- gsub("\\(Intercept\\)", "Intercept", labels_new)
labels_new <- gsub("age", "Age", labels_new)
labels_new <- gsub("sex", "Sex", labels_new)
labels_new <- gsub("education", "Education", labels_new)
labels_new <- gsub("occupation", "Occupation", labels_new)
labels_new <- gsub("smartphone", "Smartphone", labels_new)
labels_new <- gsub("device", "Device", labels_new)
tab[,1] <- labels_new

xtab <- xtable(tab,
               type="latex",
               label=paste("tab:socdem"),
               caption=paste("Sociodemographic and related variables"),
               align=c("lllccc"),
               #align=c("lp{1in} ", rep("rp{1in} ", ncol(tab2)))
)

output <- print(xtab,
                include.rownames=F,
                include.colnames=F,
                caption.placement = "top",
                file="")

output <- gsub("\\begin{table}", "\\begin{table*}", output, fixed=T)
output <- gsub("\\end{table}", "\\end{table*}", output, fixed=T)

cat(output)

cat(output, file="output/w1/tab_socdem.tex")