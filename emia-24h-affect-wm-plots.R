source("emia-utils.R")
m_valence_sub <- readRDS(paste0(outputdir, "m_valence_sub", ".RDS"))
m_energeticarousal_sub <- readRDS(paste0(outputdir, "m_energeticarousal_sub", ".RDS"))
m_calmness_sub <- readRDS(paste0(outputdir, "m_calmness_sub", ".RDS"))
m_wm_sub <- readRDS(paste0(outputdir, "m_wm_sub", ".RDS"))

# make a grid to loop plots
sub_models   <- c("m_valence_sub", "m_energeticarousal_sub", "m_calmness_sub")
resp         <- c("Valence", "Arousal", "Calmness")
level        <- c("between", "within")
level_labels <- c("Between-person ", "Within-person ")
parts        <- c("Sleep", "Sedentary", "Standing", "LPA", "MVPA")
part_labels  <- c("Sleep", "Sedentary", "Standing", "Light Physical Activity", "Moderate-to-Vigorous Physical Activity")

rg <- expand.grid.df(data.frame(sub_models, resp), 
                     data.frame(level, level_labels),
                     data.frame(parts, part_labels))
rg <- rg[order(rg$level, rg$sub_models), ]

# set up plots ----------------
col <- c(`Sleep` = "#456691",
         `Sedentary` = "#8AAFCA",
         `Standing` = "#ea967c", 
         `LPA` = "#f5c98e", #B87474 
         `MVPA` = "#7b906f") #ba6c6e  

colf <- c(`Sleep` = "#8399AE",
          `Sedentary` = "#A1B2C2",
          `Standing` = "#dfa398",
          `LPA` = "#FAD899",
          `MVPA` = "#839A7F") #C08585 

alpha <- 1/10

wes_palette("Cavalcanti1", 10, type = "continuous")
as.character(wes_palette("Cavalcanti1", 10, type = "continuous"))
pal_jco()(6)

protan <- dichromat::dichromat(col, type = "protan")
deutan <- dichromat::dichromat(col, type = "deutan")
tritan <- dichromat::dichromat(col, type = "tritan")

# plot for comparison
layout(matrix(1:4, nrow = 4)); par(mar = rep(1, 4))
recolorize::plotColorPalette(col, main = "Trichromacy")
recolorize::plotColorPalette(protan, main = "Protanopia")
recolorize::plotColorPalette(deutan, main = "Deutanopia")
recolorize::plotColorPalette(tritan, main = "Tritanopia")

# between -------------------
sub_models_b <- list()

for(i in seq_along(sub_models)) {
  
  model_tmp <- get(sub_models[[i]])
  
  model_tmp <- as.data.table(summary(model_tmp, delta = c(-30:30), level = "between", digits = "asis"))
  
  model_tmp[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  # model_tmp[, From := ifelse(From == "Sedentary", "Sedentary", From)]
  # model_tmp[, From := ifelse(From == "Standing", "Standing", From)]
  # model_tmp[, From := ifelse(From == "LPA", "LPA", From)]
  # model_tmp[, From := ifelse(From == "MVPA", "MVPA", From)]
  model_tmp[, From := factor(From, ordered = TRUE,
                             levels = c("Sleep",
                                        "Sedentary",
                                        "Standing",
                                        "LPA",
                                        "MVPA"))]
  
  model_tmp[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  # model_tmp[, To := ifelse(To == "Sedentary", "Sedentary", To)]
  # model_tmp[, To := ifelse(To == "Standing", "Standing", To)]
  # model_tmp[, To := ifelse(To == "LPA", "LPA", To)]
  # model_tmp[, To := ifelse(To == "MVPA", "MVPA", To)]
  model_tmp[, To := factor(To, ordered = TRUE,
                           levels = c("Sleep",
                                      "Sedentary",
                                      "Standing",
                                      "LPA",
                                      "MVPA"))]
  
  model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
  model_tmp[, Sig := NA]
  model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-28, 28), "*", "")]
  
  sub_models_b[[i]] <- model_tmp
}
names(sub_models_b) <- (sub_models)

emia_h24_affect_plot_b <- foreach(i = 1:15,
                             .packages = "multilevelcoda") %dopar% {
                               
                               ggplot(sub_models_b[[rg[i, "sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                      aes(x = Delta, y = Mean)) +
                                 geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                 geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                 geom_ribbon(aes(ymin = CI_low,
                                                 ymax = CI_high, fill = From),
                                             alpha = alpha) +
                                 geom_line(aes(colour = From), linewidth = 1) +
                                 geom_text(aes(label = Sig, colour = From), 
                                           size = 6, 
                                           nudge_x = 0, nudge_y = 0.25, 
                                           show.legend = FALSE) +
                                 scale_colour_manual(values = col) +
                                 scale_fill_manual(values = colf) +
                                 labs(x = paste0("Difference in ", rg[i, "parts"], " at Between-person Level"),
                                      y = paste0("Estimated Difference")) +
                                 facet_wrap(ggplot2::vars(From, To),
                                            labeller = label_bquote(cols = .(as.character(From)) %<-% minutes %->% .(as.character(To))),
                                            strip.position = "bottom", ncol = 4) +
                                 scale_x_continuous(limits = c(-33, 33),
                                                    breaks = c(-30, 0, 30),
                                                    labels = c(30, 0, 30)) +
                                 scale_y_continuous(limits = c(-10, 10),
                                                    breaks = c(-10, -5, 0, 5, 10)
                                                    # ,
                                                    # sec.axis = sec_axis(~ . / rg[i, "smean"], name = paste0("Standardised Estimated Difference"))
                                 ) +
                                 hrbrthemes::theme_ipsum() +
                                 theme(
                                   axis.ticks         = element_blank(),
                                   panel.background   = element_blank(),
                                   panel.border       = element_blank(),
                                   panel.grid.major   = element_blank(),
                                   panel.grid.minor   = element_blank(),
                                   plot.background    = element_rect(fill = "transparent", colour = NA),
                                   strip.background   = element_rect(fill = "transparent", colour = NA),
                                   strip.text         = element_text(size = 11, hjust = .5),
                                   strip.placement    = "outside",
                                   axis.title.x       = element_blank(),
                                   axis.title.y       = element_text(size = 12, hjust = .5),
                                   axis.title.y.right = element_text(size = 12, hjust = .5, angle = 270),
                                   # plot.title         = element_text(size = 12, face = "bold"),
                                   # plot.title.position= "plot",
                                   plot.margin        = margin(.5, .5, .5, .5, "cm"),
                                   legend.position    = "none"
                                 )
                             }

# wo facet
emia_h24_affect_plot_b_v2 <- foreach(i = 1:15,
                             .packages = "multilevelcoda") %dopar% {

                               ggplot(sub_models_b[[rg[i, "sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])],
                                      aes(x = Delta, y = Mean)) +
                                 geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                 geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +
                                 geom_ribbon(aes(ymin = CI_low,
                                                 ymax = CI_high, fill = From),
                                             alpha = alpha) +
                                 geom_line(aes(colour = From), linewidth = 1) +
                                 geom_text(aes(label = Sig, colour = From),
                                           size = 6,
                                           position = ggpp::position_nudge_center(center_x = 0, x = 3,
                                                                                  y = 0.5),
                                           show.legend = FALSE) +
                                 scale_colour_manual(values = col) +
                                 scale_fill_manual(values = colf) +
                                 labs(x = paste0("Difference in ", rg[i, "parts"], " at Between-person Level"),
                                      y = paste0("Estimated Difference in ", rg[i, "resp"])) +
                                 # facet_wrap(ggplot2::vars(From, To),
                                 #            labeller = label_bquote(cols = .(as.character(From)) %<-% minutes %->% .(as.character(To))),
                                 #            strip.position = "bottom", ncol = 4) +
                                 scale_x_continuous(limits = c(-33, 33),
                                                    breaks = c(-30, 0, 30)) +
                                 scale_y_continuous(limits = c(-10, 10),
                                                    breaks = c(-5, 0, 5)
                                                    #                    # ,
                                                    #                    # sec.axis = sec_axis(~ . / rg[i, "smean"], name = paste0("Standardised Estimated Difference"))
                                 ) +
                                 hrbrthemes::theme_ipsum() +
                                 theme(
                                   axis.ticks         = element_blank(),
                                   panel.background   = element_blank(),
                                   panel.border       = element_blank(),
                                   panel.grid.major   = element_blank(),
                                   panel.grid.minor   = element_blank(),
                                   plot.background    = element_rect(fill = "transparent", colour = NA),
                                   strip.background   = element_rect(fill = "transparent", colour = NA),
                                   strip.text         = element_text(size = 11, hjust = .5),
                                   strip.placement    = "outside",
                                   axis.title.x       = element_blank(),
                                   axis.title.y       = element_text(size = 12, hjust = .5),
                                   axis.title.y.right = element_text(size = 12, hjust = .5, angle = 270),
                                   # plot.title         = element_text(size = 12, face = "bold"),
                                   # plot.title.position= "plot",
                                   plot.margin        = margin(.5, .5, .5, .5, "cm"),
                                   legend.position    = "none"
                                 )
                             }

names(emia_h24_affect_plot_b) <- foreach(i = 1:15) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocations of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}

saveRDS(emia_h24_affect_plot_b, paste0(outputdir, "emia_h24_affect_plot_b", ".RDS"))

# within -------------------
sub_models_w <- list()

for(i in seq_along(sub_models)) {
  
  model_tmp <- get(sub_models[[i]])
  
  model_tmp <- as.data.table(summary(model_tmp, delta = c(-30:30), level = "within", digits = "asis"))
  
  model_tmp[, From := ifelse(From == "SleepInterval", "Sleep", From)]
  # model_tmp[, From := ifelse(From == "Sedentary", "Sedentary", From)]
  # model_tmp[, From := ifelse(From == "Standing", "Standing", From)]
  # model_tmp[, From := ifelse(From == "LPA", "LPA", From)]
  # model_tmp[, From := ifelse(From == "MVPA", "MVPA", From)]
  model_tmp[, From := factor(From, ordered = TRUE,
                             levels = c("Sleep",
                                        "Sedentary",
                                        "Standing",
                                        "LPA",
                                        "MVPA"))]
  
  model_tmp[, To := ifelse(To == "SleepInterval", "Sleep", To)]
  # model_tmp[, To := ifelse(To == "Sedentary", "Sedentary", To)]
  # model_tmp[, To := ifelse(To == "Standing", "Standing", To)]
  # model_tmp[, To := ifelse(To == "LPA", "LPA", To)]
  # model_tmp[, To := ifelse(To == "MVPA", "MVPA", To)]
  model_tmp[, To := factor(To, ordered = TRUE,
                           levels = c("Sleep",
                                      "Sedentary",
                                      "Standing",
                                      "LPA",
                                      "MVPA"))]
  
  
  model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
  model_tmp[, Sig := NA]
  model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-28, 28), "*", "")]
  
  sub_models_w[[i]] <- model_tmp
}
names(sub_models_w) <- (sub_models)

emia_h24_affect_plot_w <- foreach(i = 16:30,
                             .packages = "multilevelcoda") %dopar% {
                               
                               ggplot(sub_models_w[[rg[i, "sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                      aes(x = Delta, y = Mean)) +
                                 geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                 geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                 geom_ribbon(aes(ymin = CI_low,
                                                 ymax = CI_high, fill = From),
                                             alpha = alpha) +
                                 geom_line(aes(colour = From), linewidth = 1) +
                                 geom_text(aes(label = Sig, colour = From), 
                                           size = 6, 
                                           nudge_x = 0, nudge_y = 0.25, 
                                           show.legend = FALSE) +
                                 scale_colour_manual(values = col) +
                                 scale_fill_manual(values = colf) +
                                 labs(x = paste0("Difference in ", rg[i, "parts"], " at Within-person Level"),
                                      y = paste0("Estimated Change")) +
                                 facet_wrap(ggplot2::vars(From, To),
                                            labeller = label_bquote(cols = .(as.character(From)) %<-% minutes %->% .(as.character(To))),
                                            strip.position = "bottom", ncol = 4) +
                                 scale_x_continuous(limits = c(-33, 33),
                                                    breaks = c(-30, 0, 30),
                                                    labels = c(30, 0, 30 )) +
                                 scale_y_continuous(limits = c(-2.75, 2.5),
                                                    breaks = c(-2, -1, 0, 1, 2)
                                                    # ,
                                                    # sec.axis = sec_axis(~ . / rg[i, "smean"], name = paste0("Standardised Estimated Difference"))
                                 ) +
                                 hrbrthemes::theme_ipsum() +
                                 theme(
                                   axis.ticks         = element_blank(),
                                   panel.background   = element_blank(),
                                   panel.border       = element_blank(),
                                   panel.grid.major   = element_blank(),
                                   panel.grid.minor   = element_blank(),
                                   plot.background    = element_rect(fill = "transparent", colour = NA),
                                   strip.background   = element_rect(fill = "transparent", colour = NA),
                                   strip.text         = element_text(size = 11, hjust = .5),
                                   strip.placement    = "outside",
                                   axis.title.x       = element_blank(),
                                   axis.title.y       = element_text(size = 12, hjust = .5),
                                   axis.title.y.right = element_text(size = 12, hjust = .5, angle = 270),
                                   # plot.title         = element_text(size = 12, face = "bold"),
                                   # plot.title.position= "plot",
                                   plot.margin        = margin(.5, .5, .5, .5, "cm"),
                                   legend.position    = "none"
                                 )
                             }


names(emia_h24_affect_plot_w) <- foreach(i = 16:30) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocations of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}

saveRDS(emia_h24_affect_plot_w, paste0(outputdir, "emia_h24_affect_plot_w", ".RDS"))

## save -----------
# valence
grDevices::cairo_pdf(
  file = paste0(outputdir, "emia_24h_valence", ".pdf"),
  width = 10,
  height = 12,
)
ggarrange(emia_h24_affect_plot_b[["Between-person Reallocations of Standing and Valence"]], 
          emia_h24_affect_plot_b[["Between-person Reallocations of Light Physical Activity and Valence"]], 
          emia_h24_affect_plot_w[["Within-person Reallocations of Moderate-to-Vigorous Physical Activity and Valence"]], 
          nrow = 3, legend = "none",
          labels = c(
            "A. Between-person Reallocations of Standing and Valence",
            "B. Between-person Reallocations of LPA and Valence",
            "C. Within-person Reallocations of MVPA and Valence"
            ),
          hjust = 0,
          font.label = list(size = 13, color = "black", family = "Arial Narrow")
)
dev.off()

# arousal
grDevices::cairo_pdf(
  file = paste0(outputdir, "emia_24h_arousal", ".pdf"),
  width = 10,
  height = 4,
)
ggarrange(emia_h24_affect_plot_w[["Within-person Reallocations of Moderate-to-Vigorous Physical Activity and Arousal"]], 
          nrow = 1, legend = "none",
          labels = c(
            "A. Within-person Reallocations of MVPA and Arousal"
          ),
          hjust = 0,
          font.label = list(size = 13, color = "black", family = "Arial Narrow")
)
dev.off()


# calmness
grDevices::cairo_pdf(
  file = paste0(outputdir, "emia_24h_calmness", ".pdf"),
  width = 10,
  height = 8,
)
ggarrange(emia_h24_affect_plot_b[["Between-person Reallocations of Standing and Calmness"]], 
          emia_h24_affect_plot_w[["Within-person Reallocations of Light Physical Activity and Valence"]], 
          nrow = 2, legend = "none",
          labels = c(
            "A. Between-person Reallocations of Standing and Calmness",
            "B. Within-person Reallocations of LPA and Calmness"
          ),
          hjust = 0,
          font.label = list(size = 13, color = "black", family = "Arial Narrow")
)
dev.off()

# calmness + arousal
grDevices::cairo_pdf(
  file = paste0(outputdir, "emia_24h_calmness_arousal", ".pdf"),
  width = 10,
  height = 12,
)
ggarrange(emia_h24_affect_plot_b[["Between-person Reallocations of Standing and Calmness"]], 
          emia_h24_affect_plot_w[["Within-person Reallocations of Light Physical Activity and Valence"]], 
          emia_h24_affect_plot_w[["Within-person Reallocations of Moderate-to-Vigorous Physical Activity and Arousal"]], 
          nrow = 3, legend = "none",
          labels = c(
            "A. Between-person Reallocations of Standing and Calmness",
            "B. Within-person Reallocations of LPA and Calmness",
            "C. Within-person Reallocations of MVPA and Arousal"
          ),
          hjust = 0,
          font.label = list(size = 13, color = "black", family = "Arial Narrow")
)
dev.off()

