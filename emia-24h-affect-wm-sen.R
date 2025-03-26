source("emia-data.R")

## brmcoda clr mvpa ---------------------------
m_valence_mvpa_sen <- brmcoda(clrsen_mvpa,
                          valence ~
                            bilr1 + bilr2 + bilr3 + bilr4 +
                            wilr1 + wilr2 + wilr3 + wilr4 +
                            Wvalence_lag + ValidDuration +
                            sex + BMI + Age +
                            (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wvalence_lag | ID),
                          # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                          backend = "cmdstanr")
saveRDS(m_valence_mvpa_sen, paste0(outputdir, "m_valence_mvpa_sen", ".RDS"))

m_energeticarousal_mvpa_sen <- brmcoda(clrsen_mvpa,
                                   energeticarousal ~
                                     bilr1 + bilr2 + bilr3 + bilr4 +
                                     wilr1 + wilr2 + wilr3 + wilr4 +
                                     Wenergeticarousal_lag + ValidDuration +
                                     sex + BMI + Age +
                                     (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wenergeticarousal_lag | ID),
                                   # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                                   backend = "cmdstanr")
saveRDS(m_energeticarousal_mvpa_sen, paste0(outputdir, "m_energeticarousal_mvpa_sen", ".RDS"))

m_calmness_mvpa_sen <- brmcoda(clrsen_mvpa,
                           calmness ~
                             bilr1 + bilr2 + bilr3 + bilr4 +
                             wilr1 + wilr2 + wilr3 + wilr4 +
                             Wcalmness_lag + ValidDuration +
                             sex + BMI + Age +
                             (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wcalmness_lag | ID),
                           # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                           backend = "cmdstanr")
saveRDS(m_calmness_mvpa_sen, paste0(outputdir, "m_calmness_mvpa_sen", ".RDS"))

m_wm_mvpa_sen <- brmcoda(clrsen_mvpa,
                     WM_score ~
                       bilr1 + bilr2 + bilr3 + bilr4 +
                       wilr1 + wilr2 + wilr3 + wilr4 +
                       WWM_score_lag + ValidDuration +
                       sex + BMI + Age +
                       (1 + wilr1 + wilr2 + wilr3 + wilr4 + WWM_score_lag | ID),
                     # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr")
saveRDS(m_wm_mvpa_sen, paste0(outputdir, "m_wm_mvpa_sen", ".RDS"))

## brmcoda clr sb ---------------------------
m_valence_sb_sen <- brmcoda(clrsen_sb,
                        valence ~
                          bilr1 + bilr2 + bilr3 + bilr4 +
                          wilr1 + wilr2 + wilr3 + wilr4 +
                          Wvalence_lag + ValidDuration +
                          sex + BMI + Age +
                          (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wvalence_lag | ID),
                        # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr")
saveRDS(m_valence_sb_sen, paste0(outputdir, "m_valence_sb_sen", ".RDS"))

m_energeticarousal_sb_sen <- brmcoda(clrsen_sb,
                                 energeticarousal ~
                                   bilr1 + bilr2 + bilr3 + bilr4 +
                                   wilr1 + wilr2 + wilr3 + wilr4 +
                                   Wenergeticarousal_lag + ValidDuration +
                                   sex + BMI + Age +
                                   (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wenergeticarousal_lag | ID),
                                 # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                                 backend = "cmdstanr")
saveRDS(m_energeticarousal_sb_sen, paste0(outputdir, "m_energeticarousal_sb_sen", ".RDS"))

m_calmness_sb_sen <- brmcoda(clrsen_sb,
                         calmness ~
                           bilr1 + bilr2 + bilr3 + bilr4 +
                           wilr1 + wilr2 + wilr3 + wilr4 +
                           Wcalmness_lag + ValidDuration +
                           sex + BMI + Age +
                           (1 + wilr1 + wilr2 + wilr3 + wilr4 + Wcalmness_lag | ID),
                         # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                         backend = "cmdstanr")
saveRDS(m_calmness_sb_sen, paste0(outputdir, "m_calmness_sb_sen", ".RDS"))

m_wm_sb_sen <- brmcoda(clrsen_sb,
                   WM_score ~
                     bilr1 + bilr2 + bilr3 + bilr4 +
                     wilr1 + wilr2 + wilr3 + wilr4 +
                     WWM_score_lag + ValidDuration +
                     sex + BMI + Age +
                     (1 + wilr1 + wilr2 + wilr3 + wilr4 + WWM_score_lag | ID),
                   # iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr")
saveRDS(m_wm_sb_sen, paste0(outputdir, "m_wm_sb_sen", ".RDS"))


# model summary -----------------
m_valence_mvpa_sen <- readRDS(paste0(outputdir, "m_valence_mvpa_sen", ".RDS"))
m_energeticarousal_mvpa_sen <- readRDS(paste0(outputdir, "m_energeticarousal_mvpa_sen", ".RDS"))
m_calmness_mvpa_sen <- readRDS(paste0(outputdir, "m_calmness_mvpa_sen", ".RDS"))
m_wm_mvpa_sen <- readRDS(paste0(outputdir, "m_wm_mvpa_sen", ".RDS"))

summary(m_valence_mvpa_sen)
summary(m_energeticarousal_mvpa_sen)
summary(m_calmness_mvpa_sen)
summary(m_wm_mvpa_sen)

m_valence_sb_sen <- readRDS(paste0(outputdir, "m_valence_sb_sen", ".RDS"))
m_energeticarousal_sb_sen <- readRDS(paste0(outputdir, "m_energeticarousal_sb_sen", ".RDS"))
m_calmness_sb_sen <- readRDS(paste0(outputdir, "m_calmness_sb_sen", ".RDS"))
m_wm_sb_sen <- readRDS(paste0(outputdir, "m_wm_sb_sen", ".RDS"))

summary(m_valence_sb_sen)
summary(m_energeticarousal_sb_sen)
summary(m_calmness_sb_sen)
summary(m_wm_sb_sen)

# substitution -------------------
m_valence_sen_sub <- substitution(
  m_valence_mvpa_sen,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_valence_sen_sub, paste0(outputdir, "m_valence_sen_sub", ".RDS"))

m_energeticarousal_sen_sub <- substitution(
  m_energeticarousal_mvpa_sen,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_energeticarousal_sen_sub, paste0(outputdir, "m_energeticarousal_sen_sub", ".RDS"))

m_calmness_sen_sub <- substitution(
  m_calmness_mvpa_sen,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_calmness_sen_sub, paste0(outputdir, "m_calmness_sen_sub", ".RDS"))

m_wm_sen_sub <- substitution(
  m_wm_mvpa_sen,
  delta = c(1:60),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_wm_sen_sub, paste0(outputdir, "m_wm_sen_sub", ".RDS"))

# summary ------------------------
m_valence_sen_sub <- readRDS(paste0(outputdir, "m_valence_sen_sub", ".RDS"))
m_energeticarousal_sen_sub <- readRDS(paste0(outputdir, "m_energeticarousal_sen_sub", ".RDS"))
m_calmness_sen_sub <- readRDS(paste0(outputdir, "m_calmness_sen_sub", ".RDS"))
m_wm_sen_sub <- readRDS(paste0(outputdir, "m_wm_sen_sub", ".RDS"))

summary(m_valence_sen_sub, delta = 30)
summary(m_energeticarousal_sen_sub, delta = 30)
summary(m_calmness_sen_sub, delta = 30)
summary(m_wm_sen_sub, delta = 30)

summary(m_valence_sen_sub, delta = 5)
summary(m_energeticarousal_sen_sub, delta = 5)
summary(m_calmness_sen_sub, delta = 5)
summary(m_wm_sen_sub, delta = 5)

summary(m_valence_sen_sub, delta = 60)
summary(m_energeticarousal_sen_sub, delta = 60)
summary(m_calmness_sen_sub, delta = 60)
summary(m_wm_sen_sub, delta = 60)

