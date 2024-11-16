source("emia-data.R")

## brmcoda clr mvpa ---------------------------
m_valence_mvpa <- brmcoda(clr_mvpa,
                          valence ~
                            bilr1 + bilr2 + bilr3 + bilr4 +
                            wilr1 + wilr2 + wilr3 + wilr4 +
                            Wvalence_lag + ValidDuration +
                            sex + BMI + Age +
                            (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wvalence_lag | ID),
                          # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                          backend = "cmdstanr")
saveRDS(m_valence_mvpa, paste0(outputdir, "m_valence_mvpa", ".RDS"))

m_energeticarousal_mvpa <- brmcoda(clr_mvpa,
                                   energeticarousal ~
                                     bilr1 + bilr2 + bilr3 + bilr4 +
                                     wilr1 + wilr2 + wilr3 + wilr4 +
                                     Wenergeticarousal_lag + ValidDuration +
                                     sex + BMI + Age +
                                     (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wenergeticarousal_lag | ID),
                                   # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                                   backend = "cmdstanr")
saveRDS(m_energeticarousal_mvpa, paste0(outputdir, "m_energeticarousal_mvpa", ".RDS"))

m_calmness_mvpa <- brmcoda(clr_mvpa,
                           calmness ~
                             bilr1 + bilr2 + bilr3 + bilr4 +
                             wilr1 + wilr2 + wilr3 + wilr4 +
                             Wcalmness_lag + ValidDuration +
                             sex + BMI + Age +
                             (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wcalmness_lag | ID),
                           # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                           backend = "cmdstanr")
saveRDS(m_calmness_mvpa, paste0(outputdir, "m_calmness_mvpa", ".RDS"))

m_wm_mvpa <- brmcoda(clr_mvpa,
                     WM_score ~
                       bilr1 + bilr2 + bilr3 + bilr4 +
                       wilr1 + wilr2 + wilr3 + wilr4 +
                       WWM_score_lag + ValidDuration +
                       sex + BMI + Age +
                       (1 + wilr1 + wilr2 + wilr3 + wilr4 + WWM_score_lag | ID),
                     # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr")
saveRDS(m_wm_mvpa, paste0(outputdir, "m_wm_mvpa", ".RDS"))

# substitution -------------------
m_valence_sub <- substitution(
  m_valence_mvpa,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_valence_sub, paste0(outputdir, "m_valence_sub", ".RDS"))

m_energeticarousal_sub <- substitution(
  m_energeticarousal_mvpa,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_energeticarousal_sub, paste0(outputdir, "m_energeticarousal_sub", ".RDS"))

m_calmness_sub <- substitution(
  m_calmness_mvpa,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_calmness_sub, paste0(outputdir, "m_calmness_sub", ".RDS"))

m_wm_sub <- substitution(
  m_wm_mvpa,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_wm_sub, paste0(outputdir, "m_wm_sub", ".RDS"))

# summary ------------------------
m_valence_sub <- readRDS(paste0(outputdir, "m_valence_sub", ".RDS"))
m_energeticarousal_sub <- readRDS(paste0(outputdir, "m_energeticarousal_sub", ".RDS"))
m_calmness_sub <- readRDS(paste0(outputdir, "m_calmness_sub", ".RDS"))
m_wm_sub <- readRDS(paste0(outputdir, "m_wm_sub", ".RDS"))

summary(m_valence_sub, delta = 30)
summary(m_energeticarousal_sub, delta = 30)
summary(m_calmness_sub, delta = 30)
summary(m_wm_sub, delta = 30)

summary(m_valence_sub, delta = 5)
summary(m_energeticarousal_sub, delta = 5)
summary(m_calmness_sub, delta = 5)
summary(m_wm_sub, delta = 5)

summary(m_valence_sub, delta = 60)
summary(m_energeticarousal_sub, delta = 60)
summary(m_calmness_sub, delta = 60)
summary(m_wm_sub, delta = 60)


## brmcoda clr sb ---------------------------
m_valence_sb <- brmcoda(clr_sb,
                        valence ~
                          bilr1 + bilr2 + bilr3 + bilr4 +
                          wilr1 + wilr2 + wilr3 + wilr4 +
                          Wvalence_lag + ValidDuration +
                          sex + BMI + Age +
                          (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wvalence_lag | ID),
                        # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr")
saveRDS(m_valence_sb, paste0(outputdir, "m_valence_sb", ".RDS"))

m_energeticarousal_sb <- brmcoda(clr_sb,
                                 energeticarousal ~
                                   bilr1 + bilr2 + bilr3 + bilr4 +
                                   wilr1 + wilr2 + wilr3 + wilr4 +
                                   Wenergeticarousal_lag + ValidDuration +
                                   sex + BMI + Age +
                                   (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wenergeticarousal_lag | ID),
                                 # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                                 backend = "cmdstanr")
saveRDS(m_energeticarousal_sb, paste0(outputdir, "m_energeticarousal_sb", ".RDS"))

m_calmness_sb <- brmcoda(clr_sb,
                         calmness ~
                           bilr1 + bilr2 + bilr3 + bilr4 +
                           wilr1 + wilr2 + wilr3 + wilr4 +
                           Wcalmness_lag + ValidDuration +
                           sex + BMI + Age +
                           (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wcalmness_lag | ID),
                         # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                         backend = "cmdstanr")
saveRDS(m_calmness_sb, paste0(outputdir, "m_calmness_sb", ".RDS"))

m_wm_sb <- brmcoda(clr_sb,
                WM_score ~
                  bilr1 + bilr2 + bilr3 + bilr4 +
                  wilr1 + wilr2 + wilr3 + wilr4 +
                  WWM_score_lag + ValidDuration +
                  sex + BMI + Age +
                  (1 + wilr1 + wilr2 + wilr3 + wilr4 + WWM_score_lag | ID),
                # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                backend = "cmdstanr")
saveRDS(m_wm_sb, paste0(outputdir, "m_wm_sb", ".RDS"))


# model summary -----------------
m_valence_mvpa <- readRDS(paste0(outputdir, "m_valence_mvpa", ".RDS"))
m_energeticarousal_mvpa <- readRDS(paste0(outputdir, "m_energeticarousal_mvpa", ".RDS"))
m_calmness_mvpa <- readRDS(paste0(outputdir, "m_calmness_mvpa", ".RDS"))
m_wm_mvpa <- readRDS(paste0(outputdir, "m_wm_mvpa", ".RDS"))

summary(m_valence_mvpa)
summary(m_energeticarousal_mvpa)
summary(m_calmness_mvpa)
summary(m_wm_mvpa)

m_valence_sb <- readRDS(paste0(outputdir, "m_valence_sb", ".RDS"))
m_energeticarousal_sb <- readRDS(paste0(outputdir, "m_energeticarousal_sb", ".RDS"))
m_calmness_sb <- readRDS(paste0(outputdir, "m_calmness_sb", ".RDS"))
m_wm_sb <- readRDS(paste0(outputdir, "m_wm_sb", ".RDS"))

summary(m_valence_sb)
summary(m_energeticarousal_sb)
summary(m_calmness_sb)
summary(m_wm_sb)
