source("emia-utils.R")
emia <- read_sav("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/projects/emia/data/EMiA_actipass_ema_data_4Flora.sav")
d <- as.data.table(emia)

# survey id
d[, dayid := 1:.N, by = ID]

# next day (lead)
d[, c("SleepInterval_Lead", "Sedentary_Lead", "Standing_Lead", "LPA_Lead", "MVPA_Lead",
      "valence_Lead", "energeticarousal_Lead", "calmness_Lead", "WAI_2_xs_Lead", "Stress_1_xs_Lead", "WM_score_Lead") :=
    .SD[.(ID = ID, dayid = dayid + 1),
        .(SleepInterval, Sedentary, Standing, LPA, MVPA,
          valence, energeticarousal, calmness, WAI_2_xs, Stress_1_xs, WM_score),
        on = c("ID", "dayid")]]

# lag
d[, c("SleepInterval_Lag", "Sedentary_Lag", "Standing_Lag", "LPA_Lag", "MVPA_Lag",
      "valence_Lag", "energeticarousal_Lag", "calmness_Lag", "WAI_2_xs_Lag", "Stress_1_xs_Lag", "WM_score_Lag") :=
    .SD[.(ID = ID, dayid = dayid - 1),
        .(SleepInterval, Sedentary, Standing, LPA, MVPA,
          valence, energeticarousal, calmness, WAI_2_xs, Stress_1_xs, WM_score),
        on = c("ID", "dayid")]]

# between vs within
d[, c("BSleepInterval", "WSleepInterval") := meanDeviations(SleepInterval), by = ID]
d[, c("BSedentary", "WSedentary") := meanDeviations(Sedentary), by = ID]
d[, c("BStanding", "WStanding") := meanDeviations(Standing), by = ID]
d[, c("BLPA", "WLPA") := meanDeviations(LPA), by = ID]
d[, c("BMVPA", "WMVPA") := meanDeviations(MVPA), by = ID]

d[, c("Bvalence", "Wvalence") := meanDeviations(valence), by = ID]
d[, c("Benergeticarousal", "Wenergeticarousal") := meanDeviations(energeticarousal), by = ID]
d[, c("Bcalmness", "Wcalmness") := meanDeviations(calmness), by = ID]
d[, c("BWAI_2_xs", "WWAI_2_xs") := meanDeviations(WAI_2_xs), by = ID]
d[, c("BStress_1_xs", "WStress_1_xs") := meanDeviations(Stress_1_xs), by = ID]
d[, c("BWM_score", "WWM_score") := meanDeviations(WM_score), by = ID]

# clean movement behaviour data
parts <- c("SleepInterval", "Sedentary", "Standing", "LPA", "MVPA")
d <- d[complete.cases(d[, .(SleepInterval, Sedentary, Standing, LPA, MVPA)])]

# check 0s
zPatterns(d[, parts, with = FALSE], label = NA)
zPatterns(d[, parts, with = FALSE], label = 0)

any(apply(d[, parts, with = FALSE], 2, function(x) x == 0))
d[which(SleepInterval == 0), "ID"]
d[which(Sedentary == 0), "ID"]
d[which(Standing == 0), "ID"]
d[which(LPA == 0), "ID"]
d[which(MVPA == 0), "ID"]

# remove id 62 and 107 as missing all waking variables to impute
d <- d[ID %nin% c(62, 107)]

# impute
composition_imp <- lrEM(d[, parts, with = FALSE], label = 0, dl = rep(1, 5), ini.cov = "multRepl")
d <- cbind(d[, -parts, with = FALSE], composition_imp)

# check outcomes
hist(d$valence_Lead)
hist(d$energeticarousal_Lead)
hist(d$calmness_Lead)

hist(d$WM_score_Lead)

# composition and ilr
sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)

clr <- complr(d,
              sbp = sbp,
              parts = parts,
              idvar = "ID",
              total = 1440)

