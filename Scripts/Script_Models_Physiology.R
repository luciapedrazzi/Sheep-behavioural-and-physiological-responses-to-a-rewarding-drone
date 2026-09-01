####### PHYSIOLOGY MODELS AND PLOTS ----

# 1. Wool ----

#wool <- read.csv("Your Path/wool.csv")

wool1 <- wool |> 
  pivot_wider(
    id_cols = TagID,
    names_from = context, 
    values_from = cortisol, 
    names_prefix = "cortisol_"
  ) |>
  mutate (
    diff = cortisol_after - cortisol_before
  ) 

wool_desc <- wool |>
  mutate(
    median_gen = median (cortisol)
  ) |> group_by(context) |>
  mutate(
    median_con = median(cortisol),
    mean_con = mean(cortisol)
  ) |> ungroup()

shapiro.test(wool1$diff) #normal

t.test(wool1$cortisol_before, wool1$cortisol_after, paired = TRUE, alternative = "two.sided") #significant

## Plot ----

wool2 <- wool1 |>
  reframe(
    mean = c(mean(cortisol_before), mean(cortisol_after)),
    context = c("before", "after")
  )

wool$context <- factor(wool$context , levels = c("before", "after"), ordered = TRUE)

(wool_plot <- ggplot (wool, aes(x=context, y=cortisol)) +
    geom_boxplot(aes(fill=context),alpha = 0.5, show.legend = F)+
    scale_fill_manual(values = c("#377eb8", "#4daf4a"))+
    labs(x = "Context", y = "Wool cortisol concentration (pg/mg)") +
    theme_cowplot(font_size=12, rel_small=0.9)+
    scale_x_discrete(labels = c("after" = "After", "before" = "Before"))+
    geom_point(data=wool, aes(x=context, y=cortisol, group=TagID)) +
    geom_line(data=wool, aes(x=context, y=cortisol,group=TagID)) +
    geom_point(shape=4, data = wool2, aes(x = context, y = mean), 
               color = "white", stroke=1, size=2) 
)

# 2. Faeces ----

#faecal <- read.csv("Your Path/faecal.csv")
  
faecal <- faecal |>
  mutate(
    date = as.factor(date),
    TagID = as.factor(TagID),
    log_cort = log(cortisol),
    context = as.factor(context)
  ) |>
  group_by(context) |>
  mutate(
    mean_cort = mean(cortisol),
    median_cort = median(cortisol)
  ) |> ungroup() |>
  mutate(
    median_gen = median(cortisol)
  )

hist(faecal$log_cort) # normal distribution

lmf <- lmer(log_cort ~ context + (1|TagID) + (1|date), data=faecal)  

summary(lmf, ddf="Kenward-Roger")  #not significant

emmeansres <- as.data.frame(difflsmeans(lmf, test.effs = "context", ddf="Kenward-Roger"))

## Plot ----

predf <- ggpredict(lmf, terms = "context")

faecal$context <- factor(faecal$context, levels = c("HABITUATION", "BASELINE", "DRONE"))

(faecal_plot <- ggplot (faecal, aes(x=context, y=log_cort)) +
    geom_boxplot(aes(fill=context),alpha = 0.5)+
    scale_fill_manual(values = c("#B8377E", "#377eb8", "#4daf4a"))+
    geom_jitter(color = "black", alpha = 0.6, width = 0.2)+
    geom_point(data = predf, aes(x = x, y = predicted), 
               color = "white", size=2) +
    geom_errorbar(inherit.aes = FALSE, data = predf, 
                  aes(x=x, ymin = predicted - std.error, ymax = predicted + std.error),
                  width = 0.1, color = "white")+
    labs(x = "Context", y = "log [Faecal cortisol metabolite\nconcentration (ng/g)]") +
    theme_cowplot(font_size=12, rel_small=0.9)+
    theme(legend.position = "none")+
    scale_x_discrete(labels = c("HABITUATION" = "Habituation", "BASELINE" = "Baseline",
                                "DRONE" = "Drone"))
)

# 3. Combined plot ----
combined_physio <- plot_grid(faecal_plot, wool_plot, 
                            ncol=2,
                            labels = c("(A)", "(B)"),
                            label_x = -0.03,
                            label_size=12)

ggsave(filename = file.path(pdir, "Figure5_Physio.tif"), plot = combined_physio,
       width = 7, height = 4, dpi = 900, bg = "white")
