source("emia-utils.R")

m_valence_lead_sub <- readRDS(paste0(outputdir, "m_valence_lead_sub", ".RDS"))
m_energeticarousal_lead_sub <- readRDS(paste0(outputdir, "m_energeticarousal_lead_sub", ".RDS"))
m_calmness_lead_sub <- readRDS(paste0(outputdir, "m_calmness_lead_sub", ".RDS"))
m_wm_lead_sub <- readRDS(paste0(outputdir, "m_wm_lead_sub", ".RDS"))

# make a grid to loop plots
lead_sub_models   <- c("m_valence_lead_sub", "m_energeticarousal_lead_sub", "m_calmness_lead_sub", "m_wm_lead_sub")
resp         <- c("Valence", "Energetic Arousal", "Calmness", "Working Memory")
level        <- c("between", "within")
level_labels <- c("Between-person ", "Within-person ")
parts        <- c("Sleep", "Sedentary", "Standing", "LPA", "MVPA")
part_labels  <- c("Sleep", "Sedentary", "Standing", "Light Physical Activity", "Moderate-to-Vigorous Physical Activity")

rg <- expand.grid.df(data.frame(lead_sub_models, resp), 
                     data.frame(level, level_labels),
                     data.frame(parts, part_labels))
rg <- rg[order(rg$level, rg$lead_sub_models), ]

# set up plots ----------------
# col <- c(`Sleep` = "#456691",
#          `Sedentary` = "#8AAFCA",
#          `Standing` = "#ea967c", 
#          `LPA` = "#f5c98e", #B87474 
#          `MVPA` = "#7b906f") #ba6c6e  
# 
# colf <- c(`Sleep` = "#8399AE",
#           `Sedentary` = "#A1B2C2",
#           `Standing` = "#dfa398",
#           `LPA` = "#FAD899",
#           `MVPA` = "#839A7F") #C08585 

col <- c(`Sleep` = "#9c6755",
         `Sedentary` = "#f5c98e",
         `Standing` = "#586085",
         `LPA` = "#dfa398", #B87474 
         `MVPA` = "#659794") #ba6c6e   

colf <- c(`Sleep` = "#D1ACA5",
          `Sedentary` = "#FAD899",
          `Standing` = "#ABA2C3",
          `LPA` = "#dfa398",
          `MVPA` = "#83A192") #C08585 

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
lead_sub_models_b <- list()

for(i in seq_along(lead_sub_models)) {
  
  model_tmp <- get(lead_sub_models[[i]])
  
  model_tmp <- as.data.table(summary(model_tmp, delta = c(-60:60), level = "between", digits = "asis"))
  
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
  
  model_tmp[, Sig_5 := ifelse(sig == FALSE & Delta %in% c(-5, 5), "a", "")]
  model_tmp[, Sig_30 := ifelse(sig == FALSE & Delta %in% c(-30, 30), "b", "")]
  model_tmp[, Sig_60 := ifelse(sig == FALSE & Delta %in% c(-60, 60), "c", "")]
  
  lead_sub_models_b[[i]] <- model_tmp
}
names(lead_sub_models_b) <- (lead_sub_models)

emia_24h_lead_plot_b <- foreach(i = 1:20,
                                       .packages = "multilevelcoda") %dopar% {
                                         
                                         ggplot(lead_sub_models_b[[rg[i, "lead_sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                                aes(x = Delta, y = Mean)) +
                                           geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                           geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                           geom_ribbon(aes(ymin = CI_low,
                                                           ymax = CI_high, fill = From),
                                                       alpha = alpha) +
                                           geom_line(aes(colour = From), linewidth = 1) +
                                           geom_text(aes(label = Sig_60, colour = From), 
                                                     size = 3, 
                                                     nudge_x = 0, nudge_y = -1.5, 
                                                     show.legend = FALSE) +
                                           geom_text(aes(label = Sig_30, colour = From), 
                                                     size = 3, 
                                                     nudge_x = 0, nudge_y = -1.5, 
                                                     show.legend = FALSE) +
                                           geom_text(aes(label = Sig_5, colour = From), 
                                                     size = 3,
                                                     nudge_x = 0, nudge_y = -1.5, 
                                                     show.legend = FALSE) +
                                           scale_colour_manual(values = col) +
                                           scale_fill_manual(values = colf) +
                                           labs(x = paste0("Difference in ", rg[i, "parts"], " at Between-person Level"),
                                                y = paste0("Estimated Difference")) +
                                           facet_wrap(ggplot2::vars(From, To),
                                                      labeller = label_bquote(cols = .(as.character(From)) %<-% min %->% .(as.character(To))),
                                                      strip.position = "bottom", ncol = 4) +
                                           scale_x_continuous(limits = c(-63, 63),
                                                              breaks = c(-60, 0, 60),
                                                              labels = c(60, 0, 60)) +
                                           scale_y_continuous(limits = c(-29, 16),
                                                              breaks = c(-20, -10, 0, 10)
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

names(emia_24h_lead_plot_b) <- foreach(i = 1:20) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocations of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}
saveRDS(emia_24h_lead_plot_b, paste0(outputdir, "emia_24h_lead_plot_b", ".RDS"))

# within -------------------
lead_sub_models_w <- list()

for(i in seq_along(lead_sub_models)) {
  
  model_tmp <- get(lead_sub_models[[i]])
  
  model_tmp <- as.data.table(summary(model_tmp, delta = c(-60:60), level = "within", digits = "asis"))
  
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
  
  model_tmp[, Sig_5 := ifelse(sig == FALSE & Delta %in% c(-5, 5), "a", "")]
  model_tmp[, Sig_30 := ifelse(sig == FALSE & Delta %in% c(-30, 30), "b", "")]
  model_tmp[, Sig_60 := ifelse(sig == FALSE & Delta %in% c(-60, 60), "c", "")]
  
  lead_sub_models_w[[i]] <- model_tmp
}
names(lead_sub_models_w) <- (lead_sub_models)

emia_24h_lead_plot_w <- foreach(i = 21:40,
                                       .packages = "multilevelcoda") %dopar% {
                                         
                                         ggplot(lead_sub_models_w[[rg[i, "lead_sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                                aes(x = Delta, y = Mean)) +
                                           geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                           geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                           geom_ribbon(aes(ymin = CI_low,
                                                           ymax = CI_high, fill = From),
                                                       alpha = alpha) +
                                           geom_line(aes(colour = From), linewidth = 1) +
                                           geom_text(aes(label = Sig_60, colour = From), 
                                                     size = 3, 
                                                     nudge_x = 0, nudge_y = -0.5, 
                                                     show.legend = FALSE) +
                                           geom_text(aes(label = Sig_30, colour = From), 
                                                     size = 3, 
                                                     nudge_x = 0, nudge_y = -0.5, 
                                                     show.legend = FALSE) +
                                           geom_text(aes(label = Sig_5, colour = From), 
                                                     size = 3,
                                                     nudge_x = 0, nudge_y = -0.5, 
                                                     show.legend = FALSE) +
                                           scale_colour_manual(values = col) +
                                           scale_fill_manual(values = colf) +
                                           labs(x = paste0("Difference in ", rg[i, "parts"], " at Within-person Level"),
                                                y = paste0("Estimated Change")) +
                                           facet_wrap(ggplot2::vars(From, To),
                                                      labeller = label_bquote(cols = .(as.character(From)) %<-% min %->% .(as.character(To))),
                                                      strip.position = "bottom", ncol = 4) +
                                           scale_x_continuous(limits = c(-63, 63),
                                                              breaks = c(-60, 0, 60),
                                                              labels = c(60, 0, 60 )) +
                                           scale_y_continuous(limits = c(-10, 5),
                                                              breaks = c(-10, -5, 0, 5)
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


names(emia_24h_lead_plot_w) <- foreach(i = 21:40) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocations of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}
saveRDS(emia_24h_lead_plot_w, paste0(outputdir, "emia_24h_lead_plot_w", ".RDS"))

## supplementary materials ----
emia_24h_lead_plot_b_supp <- foreach(i = 1:20,
                                            .packages = "multilevelcoda") %dopar% {
                                              
                                              ggplot(lead_sub_models_b[[rg[i, "lead_sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                                     aes(x = Delta, y = Mean)) +
                                                geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                                geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                                geom_ribbon(aes(ymin = CI_low,
                                                                ymax = CI_high, fill = From),
                                                            alpha = alpha) +
                                                geom_line(aes(colour = From), linewidth = 1) +
                                                geom_text(aes(label = Sig_60, colour = From), 
                                                          size = 3, 
                                                          nudge_x = 0, nudge_y = -1.5, 
                                                          show.legend = FALSE) +
                                                geom_text(aes(label = Sig_30, colour = From), 
                                                          size = 3, 
                                                          nudge_x = 0, nudge_y = -1.5, 
                                                          show.legend = FALSE) +
                                                geom_text(aes(label = Sig_5, colour = From), 
                                                          size = 3,
                                                          nudge_x = 0, nudge_y = -1.5, 
                                                          show.legend = FALSE) +
                                                scale_colour_manual(values = col) +
                                                scale_fill_manual(values = colf) +
                                                labs(x = paste0("Difference in ", rg[i, "parts"], " at Between-person Level"),
                                                     y = paste0("Estimated Difference"),
                                                     title =  paste0("Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"], " at Between-person Level")) +
                                                facet_wrap(ggplot2::vars(From, To),
                                                           labeller = label_bquote(cols = .(as.character(From)) %<-% min %->% .(as.character(To))),
                                                           strip.position = "bottom", ncol = 4) +
                                                scale_x_continuous(limits = c(-63, 63),
                                                                   breaks = c(-60, 0, 60),
                                                                   labels = c(60, 0, 60)) +
                                                scale_y_continuous(limits = c(-29, 16),
                                                                   breaks = c(-20, -10, 0, 10)
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
                                                  plot.title         = element_text(size = 12, hjust = .5, face = "italic"),
                                                  plot.title.position= "plot",
                                                  plot.margin        = margin(.5, .5, .5, .5, "cm"),
                                                  legend.position    = "none"
                                                )
                                            }

names(emia_24h_lead_plot_b_supp) <- foreach(i = 1:20) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocations of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}
saveRDS(emia_24h_lead_plot_b_supp, paste0(outputdir, "emia_24h_lead_plot_b_supp", ".RDS"))

emia_24h_lead_plot_w_supp <- foreach(i = 21:40,
                                            .packages = "multilevelcoda") %dopar% {
                                              
                                              ggplot(lead_sub_models_w[[rg[i, "lead_sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                                     aes(x = Delta, y = Mean)) +
                                                geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                                geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                                geom_ribbon(aes(ymin = CI_low,
                                                                ymax = CI_high, fill = From),
                                                            alpha = alpha) +
                                                geom_line(aes(colour = From), linewidth = 1) +
                                                geom_text(aes(label = Sig_60, colour = From), 
                                                          size = 3, 
                                                          nudge_x = 0, nudge_y = -0.5, 
                                                          show.legend = FALSE) +
                                                geom_text(aes(label = Sig_30, colour = From), 
                                                          size = 3, 
                                                          nudge_x = 0, nudge_y = -0.5, 
                                                          show.legend = FALSE) +
                                                geom_text(aes(label = Sig_5, colour = From), 
                                                          size = 3,
                                                          nudge_x = 0, nudge_y = -0.5, 
                                                          show.legend = FALSE) +
                                                scale_colour_manual(values = col) +
                                                scale_fill_manual(values = colf) +
                                                labs(x = paste0("Difference in ", rg[i, "parts"], " at Within-person Level"),
                                                     y = paste0("Estimated Change"),
                                                     title =  paste0("Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"], " at Within-person Level")) +
                                                facet_wrap(ggplot2::vars(From, To),
                                                           labeller = label_bquote(cols = .(as.character(From)) %<-% min %->% .(as.character(To))),
                                                           strip.position = "bottom", ncol = 4) +
                                                scale_x_continuous(limits = c(-63, 63),
                                                                   breaks = c(-60, 0, 60),
                                                                   labels = c(60, 0, 60 )) +
                                                scale_y_continuous(limits = c(-10, 5),
                                                                   breaks = c(-10, -5, 0, 5)
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
                                                  plot.title         = element_text(size = 12, hjust = .5, face = "italic"),
                                                  plot.title.position= "plot",
                                                  plot.margin        = margin(.5, .5, .5, .5, "cm"),
                                                  legend.position    = "none"
                                                )
                                            }


names(emia_24h_lead_plot_w_supp) <- foreach(i = 21:40) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocations of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}
saveRDS(emia_24h_lead_plot_w_supp, paste0(outputdir, "emia_24h_lead_plot_w_supp", ".RDS"))
