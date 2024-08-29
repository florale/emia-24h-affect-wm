source("emia-data.R")

## brmcoda ---------------------------
m_valence <- brmcoda(clr,
                     valence ~
                       bilr1 + bilr2 + bilr3 + bilr4 +
                       wilr1 + wilr2 + wilr3 + wilr4 +
                       Wvalence_lag + ValidDuration +
                       sex + BMI + Age +
                       (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wvalence_lag | ID),
                     # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr")
saveRDS(m_valence, paste0(outputdir, "m_valence", ".RDS"))

m_energeticarousal <- brmcoda(clr,
                              energeticarousal ~
                                bilr1 + bilr2 + bilr3 + bilr4 +
                                wilr1 + wilr2 + wilr3 + wilr4 +
                                Wenergeticarousal_lag + ValidDuration +
                                sex + BMI + Age +
                                (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wenergeticarousal_lag | ID),
                              # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                              backend = "cmdstanr")
saveRDS(m_energeticarousal, paste0(outputdir, "m_energeticarousal", ".RDS"))

m_calmness <- brmcoda(clr,
                      calmness ~
                        bilr1 + bilr2 + bilr3 + bilr4 +
                        wilr1 + wilr2 + wilr3 + wilr4 +
                        Wcalmness_lag + ValidDuration +
                        sex + BMI + Age +
                        (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wcalmness_lag | ID),
                      # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                      backend = "cmdstanr")
saveRDS(m_calmness, paste0(outputdir, "m_calmness", ".RDS"))

m_wm <- brmcoda(clr,
                WM_score ~
                  bilr1 + bilr2 + bilr3 + bilr4 +
                  wilr1 + wilr2 + wilr3 + wilr4 +
                  WWM_score_lag + ValidDuration +
                  sex + BMI + Age +
                  (1 + wilr1 + wilr2 + wilr3 + wilr4 + WWM_score_lag | ID),
                # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                backend = "cmdstanr")
saveRDS(m_valence, paste0(outputdir, "m_valence", ".RDS"))

# substitution -------------------
m_valence_sub <- substitution(
  m_valence,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_valence_sub, paste0(outputdir, "m_valence_sub", ".RDS"))

m_energeticarousal_sub <- substitution(
  m_energeticarousal,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_energeticarousal_sub, paste0(outputdir, "m_energeticarousal_sub", ".RDS"))

m_calmness_sub <- substitution(
  m_calmness,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_calmness_sub, paste0(outputdir, "m_calmness_sub", ".RDS"))

m_wm_sub <- substitution(
  m_wm,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_wm_sub, paste0(outputdir, "m_wm_sub", ".RDS"))

# summary ------------------------
summary(m_valence_sub, delta = 30)
summary(m_energeticarousal_sub, delta = 30)
summary(m_calmness_sub, delta = 30)
summary(m_wm_sub, delta = 30)

