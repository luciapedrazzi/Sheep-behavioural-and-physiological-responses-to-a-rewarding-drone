####### PLOTS MODELS BEHAVIOUR ----

# 1. Pairwise distances ----

## 1.1. Day (Phase) ----

### Density ----
(density_iid <- df_iid_d |> 
    ggplot(aes(x = mean_iid, color=TagID, fill=TagID)) +
    geom_density(alpha = 0.50, position="identity") +
    labs(x="Mean pairwise distance (m)", y="Density", color = "Sheep ID",   # new legend title for color
         fill  = "Sheep ID") +
    theme_cowplot(font_size=12, rel_small=0.9) +
    theme(legend.position = "none") )

### Wind ----
prediw <- ggpredict(lm2a, terms = "mean_w")

(wind_iid <- ggplot() +
    geom_ribbon(data = prediw, aes(x = x, ymin = conf.low, ymax = conf.high), 
                alpha = 0.4, fill = "grey")+
    geom_point(data = df_iid_d, aes(x = mean_w, y = mean_iid), 
               color = "black", alpha=0.3, size=2) +
    geom_line(data = prediw, aes(x = x, y = predicted), 
              color = "#30123B", linewidth = 1.5) +
    labs(x = "Mean wind speed (m/s)", y = "Mean pairwise distance (m)") +
    theme_cowplot(font_size=12, rel_small=0.9)
)

## 1.2. Hour (Period) ----

### Rain (Hour) ----
predir <- ggpredict(lm2b, terms = "bin_p")

(rain_iid <- ggplot (df_iid_h, aes(x=bin_p, y=mean_iid)) +
    geom_boxplot(alpha = 0)+
    geom_jitter(color = "black", alpha = 0.3, width = 0.3, size = 2)+
    geom_point(data = predir, aes(x = x, y = predicted, color=x), 
               size=2.5) +
    geom_errorbar(inherit.aes = FALSE, data = predir, 
                  aes(x=x, ymin = conf.low, ymax = conf.high, color=x),
                  width = 0.1, linewidth = 1)+
    scale_color_manual(values = c("#4454c3", "#008055"))+
    labs(x = "Rainfall (mm)", y = "Mean pairwise distance (m)") +
    theme_cowplot(font_size=12, rel_small=0.9)+
    theme(legend.position = "none") +
    scale_x_discrete(labels = c("no" = "Absent", "yes" = "Present"))
)

# 2. Flock density ----

## 2.1. 1 hour (Phase) ----

### Density ----
(flock_plot <- df_flock_d_o |>
    mutate(date = as.factor(date)) |>
    ggplot(aes(x = area_den, color = date, fill = date)) +
    geom_density(alpha = 0.5, position = "identity") +
    labs(x = expression ("Mean flock density (1/m"^2*")"), y = "Density") +
    theme_cowplot(font_size = 12, rel_small = 0.9) +
    theme(legend.position = "none") +
    scale_color_viridis_d(option="turbo")+
    scale_fill_viridis_d(option="turbo")
)

### Wind ----
predfw <- ggpredict(lm2c, terms = "mean_w")

(wind_flock <- ggplot() +
    geom_ribbon(data = predfw, aes(x = x, ymin = conf.low, ymax = conf.high), 
                alpha = 0.4, fill = "grey")+
    geom_point(data = df_flock_d_o, aes(x = mean_w, y = area_den), 
               color = "black", alpha=0.3, size=2) +
    geom_line(data = predfw, aes(x = x, y = predicted), 
              color = "#30123B", linewidth = 1.5) +
    labs(x = "Mean wind speed (m/s)", y = expression ("Mean flock density (1/m"^2*")")) +
    theme_cowplot(font_size=12, rel_small=0.9)
)

### Rain ----
predfr <- ggpredict(lm2c, terms = "bin_p")

(rain_flock <- ggplot (df_flock_d_o, aes(x=bin_p, y=area_den)) +
    geom_boxplot(alpha = 0)+
    geom_jitter(color = "black", alpha = 0.3, width = 0.3, size = 2)+
    geom_point(data = predfr, aes(x = x, y = predicted, color=x), 
               size=2.5) +
    geom_errorbar(inherit.aes = FALSE, data = predfr, 
                  aes(x=x, ymin = conf.low, ymax = conf.high, color=x),
                  width = 0.1, linewidth=1)+
    scale_color_manual(values = c("#4454c3", "#008055"))+
    labs(x = "Rainfall (mm)", y = expression ("Mean flock density (1/m"^2*")")) +
    theme_cowplot(font_size=12, rel_small=0.9)+
    theme(legend.position = "none") +
    scale_x_discrete(labels = c("no" = "Absent", "yes" = "Present"))
)

### Phase ----
predfp <- ggpredict(lm2c, terms = "phase")

(phase_flock <- ggplot (df_flock_d_o, aes(x=phase, y=area_den)) +
    geom_boxplot(aes(fill=phase),alpha = 0.5)+
    scale_fill_manual(values = c("#377eb8", "#4daf4a"))+
    geom_jitter(color = "black", alpha = 0.6, width = 0.3)+
    geom_point(data = predfp, aes(x = x, y = predicted), 
               color = "white", size=2) +
    geom_errorbar(inherit.aes = FALSE, data = predfp, 
                  aes(x=x, ymin = conf.low, ymax = conf.high),
                  width = 0.1, linewidth = 1, color = "white")+
    labs(x = "Drone", y = expression ("Mean flock density (1/m"^2*")")) +
    theme_cowplot(font_size=18, rel_small=0.9)+
    theme(legend.position = "none")+
    scale_x_discrete(labels = c("yes" = "Present", "no" = "Absent"))
)

# 3. Distance travelled ----

## 3.1. Day (Phase) ----

### Density ----
(density_travel <- df_tra_d |> 
   ggplot(aes(x = tra_adj, color=TagID, fill=TagID)) +
   geom_density(alpha = 0.50, position="identity") +
   labs(x="Normalised distance travelled (m)", y="Density",
        color = "Sheep ID",   
        fill  = "Sheep ID") +
   theme_cowplot(font_size=12, rel_small=0.9) +
   theme(legend.position = "none")
)

## 3.2. Hour (Period) ----

### Period ----
df_tra_h$period <- factor(df_tra_h$period , levels = c("before", "after"), ordered = TRUE)

predpd <- ggpredict(lm2f, terms = "period")

(period_dist <- ggplot (df_tra_h, aes(x=period, y=travel_adj)) +
    geom_boxplot(aes(fill=period),alpha = 0.5)+
    scale_fill_manual(values = c("#377eb8", "#4daf4a"))+
    geom_jitter(color = "black", alpha = 0.4, width = 0.3)+
    geom_point(data = predpd, aes(x = x, y = predicted), 
               color = "white", size=2) +
    geom_errorbar(inherit.aes = FALSE, data = predpd, 
                  aes(x=x, ymin = conf.low, ymax = conf.high),
                  width = 0.1, linewidth=1,color = "white")+
    labs(x = "Drone", y = "Normalised distance travelled (m)") +
    theme_cowplot(font_size=18, rel_small=0.9)+
    theme(legend.position = "none")+
    scale_x_discrete(labels = c("before" = "Before", "after" = "After"))
)

# 4. Space use ----

## 4.1. Day (Phase) ----

### Density ----
(density_spaceuse_5 <- df_spaceuse_d_5 |> 
   ggplot(aes(x = home_adj, color=TagID, fill=TagID)) +
   geom_density(alpha = 0.50, position="identity") +
   labs(x = expression ("Normalised space use (m"^2*")"), y="Density",
        fill="Sheep ID", color="Sheep ID") +
   theme_cowplot(font_size=12, rel_small=0.9) +
   theme(legend.position = "none")
)

### Phase ----
predsp <- ggpredict(lm2g, terms = "phase")

(phase_space <- ggplot (df_spaceuse_d_5, aes(x=phase, y=home_adj)) +
    geom_boxplot(aes(fill=phase),alpha = 0.5)+
    scale_fill_manual(values = c("#377eb8", "#4daf4a"))+
    geom_jitter(color = "black", alpha = 0.4, width = 0.3)+
    geom_point(data = predsp, aes(x = x, y = predicted), 
               color = "white", size=2) +
    geom_errorbar(inherit.aes = FALSE, data = predsp, 
                  aes(x=x, ymin = conf.low, ymax = conf.high),
                  width = 0.1, linewidth = 1, color = "white")+
    labs(x = "Drone", y = expression ("Normalised space use (m"^2*")")) +
    theme_cowplot(font_size=18, rel_small=0.9)+
    theme(legend.position = "none")+
    scale_x_discrete(labels = c("yes" = "Present", "no" = "Absent"))
)

# 5. Combined plots ----
## 5.1. Densities ----
combined_dens <- plot_grid(
  density_iid,
  flock_plot,
  density_travel,
  density_spaceuse_5,
  ncol = 2,
  labels = c("(A)", "(B)", "(C)", "(D)"),
  label_x = -0.03,
  label_size = 12
)

ggsave(filename = file.path(pdir, "Figure3_Density.pdf"), plot = combined_dens,
       width = 7, height = 6, dpi = 900, bg = "white")

## 5.2. Drone ----

combined_drone <- plot_grid(period_dist, phase_space, phase_flock,
                            ncol=3,
                            labels = c("(A)", "(B)", "(C)"),
                            label_x = -0.1,
                            label_y = 1,
                            vjust = -1,
                            label_size = 18)

combined_drone <- combined_drone + 
  theme(plot.margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.3, unit = "in"))

ggsave(filename = file.path(pdir, "Figure4_Drone.pdf"),plot = combined_drone, 
       width = 11, height = 5, dpi = 900, bg = "white")

## 5.3. Weather ----
combined_weather <- plot_grid(wind_iid, wind_flock, 
                              rain_iid, rain_flock, 
                              ncol=2,
                              labels = c("(A)", "(B)", "(C)", "(D)"),
                              label_x = -0.03,
                              label_size = 12)

ggsave(filename = file.path(pdir, "Figure6_Weather.pdf"), plot = combined_weather,
       width = 7, height = 6, dpi = 900, bg = "white")
