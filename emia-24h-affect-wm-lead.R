source("emia-data.R")

## brmcoda models
m_valence_lead <- brmcoda(clr,
                          valence_lead ~
                            bilr1 + bilr2 + bilr3 + bilr4 +
                            wilr1 + wilr2 + wilr3 + wilr4 +
                            ValidDuration +
                            sex + BMI + Age +
                            (1 + wilr1 + wilr2 + wilr3 + wilr4 | ID),
                          # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                          backend = "cmdstanr")
saveRDS(m_valence_lead, paste0(outputdir, "m_valence_lead", ".RDS"))

m_energeticarousal_lead <- brmcoda(clr,
                                   energeticarousal_lead ~
                                     bilr1 + bilr2 + bilr3 + bilr4 +
                                     wilr1 + wilr2 + wilr3 + wilr4 +
                                     ValidDuration +
                                     sex + BMI + Age +
                                     (1 + wilr1 + wilr2 + wilr3 + wilr4 | ID),
                                   # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                                   backend = "cmdstanr")
saveRDS(m_energeticarousal_lead, paste0(outputdir, "m_energeticarousal_lead", ".RDS"))

m_calmness_lead <- brmcoda(clr,
                           calmness_lead ~
                             bilr1 + bilr2 + bilr3 + bilr4 +
                             wilr1 + wilr2 + wilr3 + wilr4 +
                             ValidDuration +
                             sex + BMI + Age +
                             (1 + wilr1 + wilr2 + wilr3 + wilr4 | ID),
                           # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                           backend = "cmdstanr")
saveRDS(m_calmness_lead, paste0(outputdir, "m_calmness_lead", ".RDS"))

m_wm_lead <- brmcoda(clr,
                     WM_score_lead ~
                       bilr1 + bilr2 + bilr3 + bilr4 +
                       wilr1 + wilr2 + wilr3 + wilr4 +
                       ValidDuration +
                       sex + BMI + Age +
                       (1 + wilr1 + wilr2 + wilr3 + wilr4 | ID),
                     # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr")
saveRDS(m_valence_lead, paste0(outputdir, "m_valence_lead", ".RDS"))

# substitution -------------------
m_valence_lead_sub <- substitution(
  m_valence_lead,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_valence_lead_sub, paste0(outputdir, "m_valence_lead_sub", ".RDS"))

m_energeticarousal_lead_sub <- substitution(
  m_energeticarousal_lead,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_energeticarousal_lead_sub, paste0(outputdir, "m_energeticarousal_lead_sub", ".RDS"))

m_calmness_lead_sub <- substitution(
  m_calmness_lead,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_calmness_lead_sub, paste0(outputdir, "m_calmness_lead_sub", ".RDS"))

m_wm_lead_sub <- substitution(
  m_wm_lead,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_wm_lead_sub, paste0(outputdir, "m_wm_lead_sub", ".RDS"))

# summary ------------------------
summary(m_valence_lead_sub, delta = 30)
summary(m_energeticarousal_lead_sub, delta = 30)
summary(m_calmness_lead_sub, delta = 30)
summary(m_wm_lead_sub, delta = 30)

