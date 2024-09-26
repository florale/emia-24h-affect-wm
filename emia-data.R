source("emia-utils.R")
emia <- read_sav("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/projects/emia/data/EMiA_actipass_ema_data_4Flora_reduced.sav")
d <- as.data.table(emia)

# survey id
d[, dayid := 1:.N, by = ID]

# next day (lead)
d[, c("SleepInterval_lead", "Sedentary_lead", "Standing_lead", "LPA_lead", "MVPA_lead",
      "valence_lead", "energeticarousal_lead", "calmness_lead", "WAI_2_xs_lead", "Stress_1_xs_lead", "WM_score_lead") :=
    .SD[.(ID = ID, dayid = dayid + 1),
        .(SleepInterval, Sedentary, Standing, LPA, MVPA,
          valence, energeticarousal, calmness, WAI_2_xs, Stress_1_xs, WM_score),
        on = c("ID", "dayid")]]

# lag
d[, c("SleepInterval_lag", "Sedentary_lag", "Standing_lag", "LPA_lag", "MVPA_lag",
      "valence_lag", "energeticarousal_lag", "calmness_lag", "WAI_2_xs_lag", "Stress_1_xs_lag", "WM_score_lag") :=
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

d[, c("Bvalence_lag", "Wvalence_lag") := meanDeviations(valence_lag), by = ID]
d[, c("Benergeticarousal_lag", "Wenergeticarousal_lag") := meanDeviations(energeticarousal_lag), by = ID]
d[, c("Bcalmness_lag", "Wcalmness_lag") := meanDeviations(calmness_lag), by = ID]
d[, c("BWAI_2_xs_lag", "WWAI_2_xs_lag") := meanDeviations(WAI_2_xs_lag), by = ID]
d[, c("BStress_1_xs_lag", "WStress_1_xs_lag") := meanDeviations(Stress_1_xs_lag), by = ID]
d[, c("BWM_score_lag", "WWM_score_lag") := meanDeviations(WM_score_lag), by = ID]

# clean movement behaviour data
parts <- c("SleepInterval", "Sedentary", "Standing", "LPA", "MVPA")
d <- d[complete.cases(d[, .(SleepInterval, Sedentary, Standing, LPA, MVPA)])]

# check 0s
# zPatterns(d[, parts, with = FALSE], label = NA)
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
hist(d$valence_lead)
hist(d$energeticarousal_lead)
hist(d$calmness_lead)

hist(d$WM_score_lead)

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

sbp_mvpa <- matrix(c(
  -1, -1, -1, -1, 1,
  -1, -1, -1, 1, 0,
  -1, -1, 1, 0, 0,
  -1, 1, 0, 0, 0), ncol = 5, byrow = TRUE)

clr_mvpa <- complr(d,
                   sbp = sbp_mvpa,
                   parts = parts,
                   idvar = "ID",
                   total = 1440)


sbp_sb <- matrix(c(
  -1, 1, -1, -1, -1,
  1, 0, -1, -1, -1,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)

clr_sb <- complr(d,
                   sbp = sbp_sb,
                   parts = parts,
                   idvar = "ID",
                   total = 1440)

# lag composition -----
partsl <- c("SleepInterval_lag", "Sedentary", "Standing", "LPA", "MVPA")
ds <- d[complete.cases(d[, .(SleepInterval_lag, Sedentary, Standing, LPA, MVPA)])]

zPatterns(ds[, partsl, with = FALSE], label = 0)
any(apply(ds[, partsl, with = FALSE], 2, function(x) x == 0))

ds[which(SleepInterval_lag == 0), "ID"]

# impute
compositionl_imp <- lrEM(ds[, partsl, with = FALSE], label = 0, dl = rep(1, 5), ini.cov = "multRepl")
ds <- cbind(ds[, -partsl, with = FALSE], compositionl_imp)

clrs <- complr(ds,
               sbp = sbp,
               parts = partsl,
               idvar = "ID",
               total = 1440)
