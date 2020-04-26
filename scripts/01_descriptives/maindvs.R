# Print some summary stats concerning the main DVs.

load(file="data/clean/5g_data.Rdata")

sel <- "w1"
d <- get(paste("d_", sel, sep=""))

# risks and benefits
sel_v <- c("risk_5g", "bene_pers", "bene_soc", "bene_econ")
sel_v <- c("policy_acc", "policy_vote", "policy_regul", "policy_resea")

# proportion of participants with ratings larger than 50
round(apply(d[,sel_v], 2, function(x) {prop.table(table(x > 50))}), 2)

# means
round(apply(d[,sel_v], 2, mean), 1)

# sds
round(apply(d[,sel_v], 2, sd), 1)

