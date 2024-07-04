source("emia-data.R")

m_valence <- brmcoda(clr,
                     valence_Lead ~ 
                       bilr1 + bilr2 + bilr3 + bilr4 +
                       wilr1 + wilr2 + wilr3 + wilr4 + 
                       sex + BMI + Age +
                       (1 + wilr1 + wilr2 + wilr3 + wilr4 | ID),
                     iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                     backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(m_valence, paste0(outputdir, "m_valence", ".RDS"))

m_energeticarousal <- brmcoda(clr,
                              energeticarousal_Lead ~ 
                                bilr1 + bilr2 + bilr3 + bilr4 +
                                wilr1 + wilr2 + wilr3 + wilr4 + 
                                sex + BMI + Age +
                                (1 + wilr1 + wilr2 + wilr3 + wilr4 | ID),
                              iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                              backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(m_energeticarousal, paste0(outputdir, "m_energeticarousal", ".RDS"))

m_calmness <- brmcoda(clr,
                      calmness_Lead ~ 
                        bilr1 + bilr2 + bilr3 + bilr4 +
                        wilr1 + wilr2 + wilr3 + wilr4 + 
                        sex + BMI + Age +
                        (1 + wilr1 + wilr2 + wilr3 + wilr4 | ID),
                      iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                      backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(m_calmness, paste0(outputdir, "m_calmness", ".RDS"))