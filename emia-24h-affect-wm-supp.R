source("emia-utils.R")

# concurrent ----------
m_valence_sub <- readRDS(paste0(outputdir, "m_valence_sub", ".RDS"))
m_energeticarousal_sub <- readRDS(paste0(outputdir, "m_energeticarousal_sub", ".RDS"))
m_calmness_sub <- readRDS(paste0(outputdir, "m_calmness_sub", ".RDS"))
m_wm_sub <- readRDS(paste0(outputdir, "m_wm_sub", ".RDS"))

emia_24h_sub_30_list <- lapply(list(
  m_valence_sub, m_energeticarousal_sub, m_calmness_sub, m_wm_sub
), function(X) {
  
  X <- summary(X, delta = 30)
  X[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  X[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  
  X$Sig <- between(0, X$CI_low, X$CI_high)
  X[, Sig := ifelse(Sig == FALSE & Delta %in% c(-30, 30), "Yes", "")]
  
  X[, `Estimate [95% CI]`:= paste0(Mean, " [", CI_low, ", ", CI_high, "]")]
  X <- X[, .(`Estimate [95% CI]`, Minute = Delta, From, To, Level, Sig)]
})

emia_24h_sub_5_list <- lapply(list(
  m_valence_sub, m_energeticarousal_sub, m_calmness_sub, m_wm_sub
), function(X) {
  
  X <- summary(X, delta = 5)
  X[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  X[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  
  X$Sig <- between(0, X$CI_low, X$CI_high)
  X[, Sig := ifelse(Sig == FALSE & Delta %in% c(-5, 5), "Yes", "")]
  
  X[, `Estimate [95% CI]`:= paste0(Mean, " [", CI_low, ", ", CI_high, "]")]
  X <- X[, .(`Estimate [95% CI]`, Minute = Delta, From, To, Level, Sig)]
})

emia_24h_sub_60_list <- lapply(list(
  m_valence_sub, m_energeticarousal_sub, m_calmness_sub, m_wm_sub
), function(X) {
  
  X <- summary(X, delta = 60)
  X[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  X[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  
  X$Sig <- between(0, X$CI_low, X$CI_high)
  X[, Sig := ifelse(Sig == FALSE & Delta %in% c(-60, 60), "Yes", "")]
  
  X[, `Estimate [95% CI]`:= paste0(Mean, " [", CI_low, ", ", CI_high, "]")]
  X <- X[, .(`Estimate [95% CI]`, Minute = Delta, From, To, Level, Sig)]
})

## graphs
emia_24h_plot_b_supp <- readRDS(paste0(outputdir, "emia_24h_plot_b_supp", ".RDS"))
emia_24h_plot_w_supp <- readRDS(paste0(outputdir, "emia_24h_plot_w_supp", ".RDS"))

# prospective ------------------
m_valence_lead_sub <- readRDS(paste0(outputdir, "m_valence_lead_sub", ".RDS"))
m_energeticarousal_lead_sub <- readRDS(paste0(outputdir, "m_energeticarousal_lead_sub", ".RDS"))
m_calmness_lead_sub <- readRDS(paste0(outputdir, "m_calmness_lead_sub", ".RDS"))
m_wm_lead_sub <- readRDS(paste0(outputdir, "m_wm_lead_sub", ".RDS"))

emia_24h_lead_sub_30_list <- lapply(list(
  m_valence_lead_sub, m_energeticarousal_lead_sub, m_calmness_lead_sub, m_wm_lead_sub
), function(X) {
  
  X <- summary(X, delta = 30)
  X[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  X[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  
  X$Sig <- between(0, X$CI_low, X$CI_high)
  X[, Sig := ifelse(Sig == FALSE & Delta %in% c(-30, 30), "Yes", "")]
  
  X[, `Estimate [95% CI]`:= paste0(Mean, " [", CI_low, ", ", CI_high, "]")]
  X <- X[, .(`Estimate [95% CI]`, Minute = Delta, From, To, Level, Sig)]
})

emia_24h_lead_sub_5_list <- lapply(list(
  m_valence_lead_sub, m_energeticarousal_lead_sub, m_calmness_lead_sub, m_wm_lead_sub
), function(X) {
  
  X <- summary(X, delta = 5)
  X[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  X[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  
  X$Sig <- between(0, X$CI_low, X$CI_high)
  X[, Sig := ifelse(Sig == FALSE & Delta %in% c(-5, 5), "Yes", "")]
  
  X[, `Estimate [95% CI]`:= paste0(Mean, " [", CI_low, ", ", CI_high, "]")]
  X <- X[, .(`Estimate [95% CI]`, Minute = Delta, From, To, Level, Sig)]
})

emia_24h_lead_sub_60_list <- lapply(list(
  m_valence_lead_sub, m_energeticarousal_lead_sub, m_calmness_lead_sub, m_wm_lead_sub
), function(X) {
  
  X <- summary(X, delta = 60)
  X[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  X[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  
  X$Sig <- between(0, X$CI_low, X$CI_high)
  X[, Sig := ifelse(Sig == FALSE & Delta %in% c(-60, 60), "Yes", "")]
  
  X[, `Estimate [95% CI]`:= paste0(Mean, " [", CI_low, ", ", CI_high, "]")]
  X <- X[, .(`Estimate [95% CI]`, Minute = Delta, From, To, Level, Sig)]
  
})

# graphs
emia_24h_lead_plot_b_supp <- readRDS(paste0(outputdir, "emia_24h_lead_plot_b_supp", ".RDS"))
emia_24h_lead_plot_w_supp <- readRDS(paste0(outputdir, "emia_24h_lead_plot_w_supp", ".RDS"))

