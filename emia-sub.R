source("emia-utils.R")
m_valence <- readRDS(paste0(outputdir, "m_valence", ".RDS"))
m_energeticarousal <- readRDS(paste0(outputdir, "m_energeticarousal", ".RDS"))
m_calmness <- readRDS(paste0(outputdir, "m_calmness", ".RDS"))

m_valence_sub <- substitution(
  m_valence,
  delta = c(30),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_valence_sub, paste0(outputdir, "m_valence_sub", ".RDS"))

m_energeticarousal_sub <- substitution(
  m_energeticarousal,
  delta = c(1:30),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_energeticarousal_sub, paste0(outputdir, "m_energeticarousal_sub", ".RDS"))

m_calmness_sub <- substitution(
  m_calmness,
  delta = c(1:30),
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
saveRDS(m_calmness_sub, paste0(outputdir, "m_calmness_sub", ".RDS"))
