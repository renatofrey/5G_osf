# This script loads the raw datasets (cannot be provided publicly to guarantee participants' anonymity) and prepares the clean dataset.

# # select wave to be prepared
for (sel in c("w1", "w2")) {
  
  print(paste("Preparing:", sel))
  
  # read data
  p <- read.csv(paste("data/raw/", sel, "/participants.csv", sep=""))
  r <- read.csv(paste("data/raw/", sel, "/ratings.csv", sep=""))
  i <- read.csv(paste("data/raw/", sel, "/items.csv", sep=""))
  
  if (sel != "w1") p$pid <- as.character(p$pid)
  
  # filter correct and complete cases
  p <- subset(p, pid >= 100000 & is.element(status, c("done", "referred")))
  pids <- p$pid
  print(paste(length(pids), "participants completed study."))
  
  # only retain participants who correctly responded to attention check (using a small margin of error: <= 10)
  pids_ok <- subset(r, item == "attent" & rating <= 10)$pid
  n_failed <- sum(!is.element(pids, pids_ok))
  print(paste(n_failed, "participants failed attention check"))
  pids <- intersect(pids, pids_ok)
  print(paste(length(pids), "participants remaining for analysis."))
  
  # subset clean datasets
  p <- subset(p, is.element(pid, pids))
  r <- subset(r, is.element(pid, pids))
  
  # remove ratings where item name was not saved
  r <- subset(r, item != "")
  r$item <- droplevels(r$item)
  
  # generate a key to anonymize pids
  if (sel == "w1") {
    key <- data.frame(old=p$pid, new=substr(p$sid, 1, 6))
    write.csv(key, file="data/local/key.csv")
    p$pid <- key[match(p$pid, key$old), "new"]
    r$pid <- key[match(r$pid, key$old), "new"]
  }
  if (sel == "w2") {
    pids_old <- p$pid
    key <- read.csv("data/local/key.csv", row.names=1)
    pids_new <- as.character(key[match(p$pid, key$old), "new"])
    ind_crosssect <- which(is.na(pids_new))
    pids_new[ind_crosssect] <- substr(p$sid, 1, 6)[ind_crosssect]
    p$retest <- "w2"
    p$retest[ind_crosssect] <- "no"
    p$pid <- pids_new[match(p$pid, pids_old)]
    r$pid <- pids_new[match(r$pid, pids_old)]
    
    conds <- read.csv("data/local/w2_pids.csv", row.names=1)
    p$cond <- conds[match(p$pid, conds$pid), "cond"]
  }
  
  
  # reverse inversely coded items
  ind_reverse <- is.element(r$item, c("dread1", "dread3", "unknown3", "unknown5"))
  r$rating[ind_reverse] <- 100 - r$rating[ind_reverse]
  
  # add 5G coverage based on ZIP codes
  if (sel == "w1") zip <- read.csv("data/raw/gis/19oct/cover1km.csv")
  if (sel == "w2") zip <- read.csv("data/raw/gis/20jan/cover1km.csv")
  zips <- unique(zip$PLZ)
  p$coverage <- is.element(p$zip, zips)
  
  # reverse "wrong knowledge" concerning 5G-coverage for participants who do not have coverage
  r_eval <- subset(r, item == "know_cover")
  r$item <- gsub("know_cover", "know_cover_raw", r$item)
  pid_nocov <- subset(p, coverage == F)$pid
  ind_rev <- which(is.element(r_eval$pid, pid_nocov))
  r_eval[ind_rev,"rating"] <- 100 - r_eval[ind_rev,"rating"]
  r <- rbind(r, r_eval)
  
  # filter ratings that were saved twice (there existed fewer than 10 of these cases)
  n_ratings <- tapply(r$rating, list(r$pid, r$item), length)
  n_ratings[is.na(n_ratings)] <- 0
  n_ratings <- n_ratings[which(rowSums(n_ratings > 1) > 0),]
  to_check <- apply(n_ratings, 1, function(x) {which(x > 1)})
  for (k in 1:length(to_check)) {
    curr_check <- to_check[k]
    curr_pid <- names(curr_check)
    for (j in 1:length(curr_check[[1]])) {
      curr_item <- names(curr_check[[1]][j])
      rows <- subset(r, pid == curr_pid & item == curr_item)
      print(rows)
      ids_rem <- rows[2:nrow(rows),"id"]
      r <- r[-which(is.element(r$id, ids_rem)),]
    }
  }
  
  # reshape to wide format
  wide <- reshape(r[,c("pid", "item", "rating")], idvar="pid", timevar="item", v.names="rating", direction="wide")
  row.names(wide) <- wide[,1]
  wide <- wide[,-1]
  colnames(wide) <- gsub("rating.", "", colnames(wide))
  wide <- wide[,order(colnames(wide))]
  cn <- colnames(wide)
  
  # create nice overview dataframe
  d <- cbind(p, wide[match(p$pid, row.names(wide)),])
  d$sex <- droplevels(d$sex)
  
  # add some approximations to factors
  d$sc_dread <- apply(d[,paste("dread", 1:5, sep="")], 1, mean, na.rm=T)
  d$sc_unkno <- apply(d[,paste("unknown", 1:5, sep="")], 1, mean, na.rm=T)
  d$sc_kns <- apply(d[,c("know_subj", "know_media")], 1, mean, na.rm=T)
  d$sc_kno <- apply(d[,c("know_radiat", "know_freq", "know_limits", "know_cover")], 1, mean, na.rm=T)
  d$sc_progr <- apply(d[,c("riskpref", "openness", "digit")], 1, mean, na.rm=T)
  d$sc_policy <- apply(d[,c("policy_regul", "policy_resea", "policy_vote", "policy_acc")], 1, mean, na.rm=T)
  d$sc_bene <- apply(d[,c("bene_pers", "bene_soc", "bene_econ")], 1, mean, na.rm=T)
  
  
  # convert education to ordinal
  d$education <- as.ordered(d$education)
  
  # remove sensitive data
  rem <- c("id", "sid", "ip", "canton", "zip", "otherrisks", "status", "t1", "t2", "rlcond", "rlrisky", "rlsafe", "rldomain", "rldomain", "rldomainoth")
  d <- d[,!is.element(colnames(d), rem)]
  r <- r[,-1]
  
  # create list of variables
  if (sel == "w1") {
    v <- tapply(i$item, list(i$module), as.character)
    v$psychframe <- v$psychframe[-which(v$psychframe == "attent")]
    v$psychframe <- v$psychframe[-which(v$psychframe == "risk_5g")]
    v$`5g` <- c(v$`5g`, "risk_5g")
    v$mms <- c("risk_5g", "bene_pers", "bene_soc", "bene_econ", "policy_acc", "policy_vote", "policy_regul", "policy_resea", paste("dread", 1:5, sep=""), paste("unknown", 1:5, sep=""), "trust", "know_subj", "know_media", "know_radiat", "know_freq", "know_limits", "know_cover", "riskpref", "openness", "digit")
  }
  if (sel == "w2") v_w2 <- c("rep_engaged", "rep_informed", "rep_changed")
  
  # select subset to be invited in W2 for longitudinal sample
  if (sel == "w1") {
    ok <- subset(p, contact == "ok")
    dim(ok)
    
    set.seed(333)
    
    # aspiring for 4*250 = 1000 complete participants in the longitudinal sample and taking into account an attrition rate of about 30%, the plan is to recruit 4*350 = 1400 participants
    N <- 1400
    
    ind <- sample(1:nrow(ok), size=N)
    w2 <- ok[ind,]
    
    # randomly assign conditions
    conds <- c("A_mgmtsum", "B_pressrel", "C_keypoints", "D_control")
    w2$cond <- sample(rep(conds, each=(N/4)))
    w2 <- w2[order(w2$cond, w2$lang),]
    
    # check
    w2$language <- droplevels(w2$language)
    Ns <- table(w2$cond, w2$language)
    Ns <- cbind(Ns, tot=rowSums(Ns))
    print(Ns)
    print(round(Ns[,"fr"] / (N/4), 2)) # ratios of french speaking p's.
    round(prop.table(table(d$language)), 2) # ratio in full dataset
    
    # add external ids
    w2$pid_ext <- key[match(w2$pid, key$new),"old"]
    rownames(w2) <- 1:nrow(w2)
    
    w2 <- w2[,c("pid", "pid_ext", "cond", "language")]
    w2_ext <- data.frame(pid_ext = w2$pid_ext)
    w2_ext$cond <- paste(substr(w2$cond, 1, 1), w2$language, sep="_")
    
    write.csv(w2, file="data/local/w2_pids.csv")
    write.csv(w2_ext, file="data/local/w2_ext.csv")
  }
  
  # assign variables to respective waves; split cross-sectional / longitudinal sample
  if (sel == "w1") {
    d_w1 <- d
    r_w1 <- r
    
    # create two extreme groups based on risk perception
    quants <- quantile(d_w1$risk_5g, c(.10,.90), na.rm=T)
    d_w1$ext_grp <- "none"
    d_w1$ext_grp[which(d_w1$risk_5g < quants[1])] <- "low"
    d_w1$ext_grp[which(d_w1$risk_5g > quants[2])] <- "high"
  }
    
  }
  if (sel == "w2") {
    d_w2_cross <- subset(d, retest == "no")
    r_w2_cross <- subset(r, is.element(pid, d_w2_cross$pid))
    d_w2_long <- subset(d, retest == "w2")
    print(nrow(d_w2_long))
    d_w2_long <- subset(d_w2_long, cond == "D_control" | letter == "yes")
    print(nrow(d_w2_long))
    r_w2_long <- subset(r, is.element(pid, d_w2_long$pid))
 
    # # add some info from W1 to longitudinal sample for direct access
    # d_w2_long$device_w1 = d_w1[pid_ind,'device']
    # d_w2_long$coverage_w1 = d_w1[pid_ind,'coverage']
    # d_w2_long$age_w1 = d_w1[pid_ind,'age']
    # d_w2_long$education_w1 = d_w1[pid_ind,'education']
    # d_w2_long$occupatation_w1 = d_w1[pid_ind,'occupation']
       
    # compute change from w1 to w2 for participants in the longitudinal sample
    var_types <- summary.default(d_w1)
    var_ind <- names(which(var_types[,"Class"] == "-none-" & var_types[,"Mode"] == "numeric"))
    
    # & rownames(var_types) != "age" & rownames(var_types) != "education"
    
    
    pid_ind <- match(d_w2_long$pid, d_w1$pid)
    d_w2_long_change <- data.frame(d_w2_long[,c("pid",
                                                "cond")],
                                   d_w2_long[,var_ind] - d_w1[pid_ind,var_ind])
    
    ind <- match(paste(r_w2_long$pid, r_w2_long$item, sep="_"),
                 paste(r_w1$pid, r_w1$item, sep="_"))
    round(prop.table(table(is.na(ind))), 3)
    r_w2_long <- cbind(r_w2_long[,c("pid", "item", "module", "rating")],
                       w2 = r_w1[ind,"rating"])
    names(r_w2_long) <- gsub("rating", "w1", names(r_w2_long))
    r_w2_long$diff <- r_w2_long$w2 - r_w2_long$w1
    
    d_w2_long$ext_grp <- d_w1[match(d_w2_long$pid, d_w1$pid), "ext_grp"]
    d_w2_long_change$ext_grp <- d_w1[match(d_w2_long_change$pid, d_w1$pid), "ext_grp"]
        
  }
  

# define colors
library(viridis)
cols <- viridis(3, begin = .80, end = .2, alpha=.6)

save(d_w1, d_w2_cross, d_w2_long, d_w2_long_change,
     r_w1, r_w2_cross, r_w2_long,
     v, v_w2, cols, quants,
     file=paste("data/clean/5g_data.Rdata", sep=""))