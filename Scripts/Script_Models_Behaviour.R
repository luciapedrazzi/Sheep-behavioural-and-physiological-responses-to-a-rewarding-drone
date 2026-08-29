####### MODELS BEHAVIOUR ----

# 0. Kendall correlation ----

## 0.1. Day level (daily means) ----
#iid_d is taken as an example

#df_iid_d <- read.csv("Your Path/df_iid_d.csv")

corrday1 <- df_iid_d |>
  select(date,phase,mean_w,mean_t,sum_p) |>
  mutate(
    date = as.Date(date),
    phase_num = as.numeric(ifelse(phase=="no", 0, 1)),
    bin_p = ifelse(sum_p>0, "yes", "no"),
    bin_p_num = case_when(
      bin_p =="no" ~ 0,
      bin_p =="yes" ~ 1
    )
  ) |>
  distinct(date, .keep_all = T) 

cor_d1 <- cor(corrday1[, c("mean_w", "mean_t", "phase_num", "bin_p_num")], use = "complete.obs", method="kendall")
corrplot(cor_d1, method = "color", addCoef.col = "black")
#no correlations

## 0.2. Day level (hourly means) ----
#flock_d is taken as an example

#df_flock_d <- read.csv("Your Path/df_flock_d.csv")

corrday2 <- df_flock_d |>
  select(timestamp,date,phase,mean_w,mean_t,sum_p) |>
  mutate(
    date = as.Date(date),
    timestamp = as.POSIXct(timestamp),
    phase_num = as.numeric(ifelse(phase=="no", 0, 1)),
    bin_p = ifelse(sum_p>0, "yes", "no"),
    bin_p_num = case_when(
      bin_p =="no" ~ 0,
      bin_p =="yes" ~ 1
    )
  ) |>
  distinct(timestamp, .keep_all = T) 

cor_d2 <- cor(corrday2[, c("mean_w", "mean_t", "phase_num", "bin_p_num")], use = "complete.obs", method="kendall")
corrplot(cor_d2, method = "color", addCoef.col = "black")
#no correlations

## 0.3. Hour level ----
#df_iid_h is taken as an example

#df_iid_h <- read.csv("Your Path/df_iid_h.csv")

corrhour <- df_iid_h |>
  select(date,period,mean_w,mean_t,sum_p) |>
  mutate(
    period_num = as.numeric(ifelse(period=="before", 0, 1)),
    bin_p = ifelse(sum_p>0, "yes", "no"),
    bin_p_num = case_when(
      bin_p =="no" ~ 0,
      bin_p =="yes" ~ 1)
  ) |> distinct(date, period, .keep_all = T)

cor_h <- cor(corrhour[, c("mean_w", "mean_t", "period_num","bin_p_num")], use = "complete.obs", method="kendall")
corrplot(cor_h, method = "color", addCoef.col = "black")
#no correlations

# 1. Pairwise distances ----

## 1.1. Day (Phase) ----

#df_iid_d <- read.csv("Your Path/df_iid_d.csv")

df_iid_d <- df_iid_d |>
  mutate (
  bin_p = ifelse(sum_p>0, "yes", "no"),
  TagID = as.factor(TagID),
  date = as.factor(date),
  phase = as.factor(phase),
  bin_p = as.factor(bin_p),
  median = median(mean_iid)
  )

str(df_iid_d)

hist(df_iid_d$mean_iid) #normally distributed

lm0a <- lmer(mean_iid ~ (1|date), data = df_iid_d)
summary(lm0a)

lm1a <- lmer(mean_iid ~ (1|TagID) + (1|date), data = df_iid_d) #crossed (not nested) random effects
summary(lm1a)

lm2a <- lmer(mean_iid ~ mean_w + mean_t + bin_p + phase +
                (1|TagID) + (1|date), data = df_iid_d)
summary(lm2a, ddf="Kenward-Roger") #phase not significant, wind significant

reslm2a <- simulateResiduals(fittedModel = lm2a, plot=T) 

check_collinearity(
  lm2a,
  component = c('all')  # no correlations
)

anova(lm0a, lm1a) #significant
ranova(lm2a)

## 1.2. Hour (Period) ----

#df_iid_h <- read.csv("Your Path/Data/df_iid_h.csv")

df_iid_h <- df_iid_h |>
  mutate (
  bin_p = ifelse(sum_p>0, "yes", "no"),
  TagID = as.factor(TagID),
  date = as.factor(date),
  period = as.factor (period),
  bin_p = as.factor(bin_p)
)

hist(df_iid_h$mean_iid) # normal distribution with a slight right tail

lm0b <- lmer (mean_iid ~ (1|date), data= df_iid_h) 
summary(lm0b)

lm1b <- lmer (mean_iid ~ (1|date) + (1|TagID), data= df_iid_h) 
summary(lm1b)

lm2b <- lmer (mean_iid ~ mean_w + mean_t + bin_p + period +
                (1|date) + (1|TagID), data= df_iid_h) 
summary(lm2b, ddf="Kenward-Roger")#period not significant, wind and rain significant

reslm2b <- simulateResiduals(fittedModel = lm2b, plot=T) 

check_collinearity(
  lm2b,
  component = c('all')  # no correlations
)

anova(lm0b, lm1b) #not significant
ranova(lm2b)

# 2. Flock density ----

## 2.1. Day (Phase) ----

#df_flock_d <- read.csv("Your Path/df_flock_d.csv")

df_flock_d <- df_flock_d |>
  mutate (
    bin_p = ifelse(sum_p>0, "yes", "no"),
    date = as.Date(date),
    phase = as.factor (phase),
    bin_p = as.factor(bin_p),
    date = as.factor(date),
    timestamp = as.POSIXct(timestamp),
    median = median(area_den)
  )

str(df_flock_d)
hist(df_flock_d$area_den) #left skewed and right tail, one massive outlier

df_flock_d_o <- df_flock_d |>
  filter(area_den < 50) #getting rid of the massive outlier

hist(df_flock_d_o$area_den) #not normally distributed

lm2c <- glmmTMB(area_den ~ mean_w + mean_t + bin_p + phase +
                  (1|date), 
                data = df_flock_d_o,
                family = Gamma(link = "log"))

summary(lm2c) #rain, wind, phase significant

reslm2c <- simulateResiduals(fittedModel = lm2c, plot=T) 

check_collinearity(
  lm2c,
  component = c('all') 
)

## 2.2. Hour (Period) ----

#df_flock_h <- read.csv("Your Path/df_flock_h.csv")

df_flock_h <- df_flock_h |>
  mutate (
    bin_p = ifelse(sum_p>0, "yes", "no"),
    date = as.factor(date),
    period = as.factor (period),
    bin_p = as.factor(bin_p)
  )

hist(df_flock_h$area_den) #not normally distributed

lm2d <- glmmTMB (area_den ~  mean_w + mean_t + bin_p + period + 
                (1|date), data= df_flock_h,
              family = Gamma(link = "log") )
summary(lm2d) #none significant

reslm2d <- simulateResiduals(fittedModel = lm2d, plot=T)

check_collinearity(
  lm2d,
  component = c('all')  # no correlations
)

# 3. Distance travelled ----

## 3.1. Day (Phase) ----

#df_tra_d <- read.csv("Your Path/df_tra_d.csv")

df_tra_d <- df_tra_d |>
  mutate (
    bin_p = ifelse(sum_p>0, "yes", "no"),
    TagID = as.factor(TagID),
    date = as.factor(date),
    phase = as.factor (phase),
    bin_p = as.factor(bin_p),
    median = median(tra_adj)
  )

str(df_tra_d)

hist(df_tra_d$tra_adj, breaks = 25) #normally distributed with slight right tail

lm0e <- lmer(tra_adj ~ (1|date), data = df_tra_d)
summary(lm0e)

lm1e <- lmer(tra_adj ~ (1|TagID) + (1|date), data = df_tra_d)
summary(lm1e)

lm2e <- lmer(tra_adj ~  mean_w + mean_t + bin_p + phase +
               (1|TagID) + (1|date), data = df_tra_d)
summary(lm2e, ddf="Kenward-Roger") #none significant (wind and temp have a trend)

reslm2e <- simulateResiduals(fittedModel = lm2e, plot=T) 

check_collinearity(
  lm2e,
  component = c('all')  # no correlations
)

anova(lm0e, lm1e) #significant
ranova(lm2e)

## 3.2. Hour (Period) ----

#df_tra_h <- read.csv("Your Path/df_tra_h.csv")

df_tra_h <- df_tra_h |>
  mutate (
    bin_p = ifelse(sum_p>0, "yes", "no"),
    TagID = as.factor(TagID),
    date = as.factor(date),
    period = as.factor (period),
    bin_p = as.factor(bin_p)
  )

hist(df_tra_h$travel_adj) # not normal, strong right skew

lm0f <- glmmTMB(travel_adj ~(1|date), data = df_tra_h,
                family = tweedie(link = "log"))
summary(lm0f)

lm1f <- glmmTMB (travel_adj ~ (1|date) + (1|TagID), data= df_tra_h, 
                 family = tweedie(link = "log")) 
summary(lm1f)

lm2f <- glmmTMB(travel_adj ~ mean_w + mean_t + bin_p + period +
                (1|date) + (1|TagID), data= df_tra_h,
                family = tweedie(link = "log")) 
summary(lm2f) #period significant, weather no

reslm2f <- simulateResiduals(fittedModel = lm2f, plot=T) 

check_collinearity(
  lm2f,
  component = c('all')  # no correlations
)

anova(lm0f, lm1f) #significant

# 4. Space use (core) ----

## 4.1. Day (Phase) ----

#df_spaceuse_d <- read.csv("Your Path/df_spaceuse_d.csv")

df_spaceuse_d_5 <- df_spaceuse_d |>
  filter(percentage==0.5)

df_spaceuse_d_5 <- df_spaceuse_d_5 |>
  mutate (
    bin_p = ifelse(sum_p>0, "yes", "no"),
    TagID = as.factor(TagID),
    date = as.factor(date),
    phase = as.factor (phase),
    bin_p = as.factor(bin_p),
    median = median (home_adj)
  )

hist(df_spaceuse_d_5$home_adj) #normally distributed

lm0g <- lmer(home_adj ~ (1|date), data = df_spaceuse_d_5)
summary(lm0g)

lm1g <- lmer(home_adj ~  (1|TagID) + (1|date), data = df_spaceuse_d_5)
summary(lm1g)

lm2g <- lmer(home_adj ~ mean_w + mean_t + bin_p + phase +
               (1|date) + (1|TagID) , data = df_spaceuse_d_5)
summary(lm2g) #phase significant

reslm2g <- simulateResiduals(fittedModel = lm2g, plot=T) 

check_collinearity(
  lm2g,
  component = c('all')  # no correlations
)

anova(lm0g, lm1g) #significant
ranova(lm2g)

# 5. Tables ----

## 5.1. Inter-individual distances ----
summary_lm2a <- lm2a |> broom.mixed::tidy() |> mutate(model = "Day level")
summary_lm2b <- lm2b |> broom.mixed::tidy() |> mutate(model = "Hour level")

iid_sum <- bind_rows(summary_lm2a, summary_lm2b) |> 
  mutate(across(where(is.numeric), ~ round(., digits = 3))) |> 
  filter(effect == "fixed") |> 
  filter(term != "(Intercept)") |> 
  mutate(term = case_match(
    term,
    "bin_pyes" ~ "Rainfall (mm)",
    "mean_w" ~ "Wind speed (m/s)",
    "mean_t" ~ "Temperature (°C)",
    "phaseyes" ~ "Drone (present)",
    "periodbefore" ~ "Drone (after)",
    .default = term # This ensures any terms you don't explicitly name stay the same
  )) |>
  rename(
    Model = model,
    `Explanatory variable` = term,
    `Estimate` = estimate,
    `Standard error` = std.error,
    `T-value` = statistic,
    `P-value` = p.value
  ) |>  select(Model, `Explanatory variable`, Estimate, `Standard error`, `T-value`, `P-value`) |> 
  flextable() |> 
  autofit() 

## 5.2. Flock density ----
summary_lm2c <- lm2c |> broom.mixed::tidy() |> mutate(model = "Day level")
summary_lm2d <- lm2d |> broom.mixed::tidy() |> mutate(model = "Hour level")

flock_sum <- bind_rows(summary_lm2c, summary_lm2d) |> 
  mutate(across(where(is.numeric), ~ round(., digits = 3))) |> 
  filter(effect == "fixed") |> 
  filter(term != "(Intercept)") |> 
  mutate(term = case_match(
    term,
    "bin_pyes" ~ "Rainfall (mm)",
    "mean_w" ~ "Wind speed (m/s)",
    "mean_t" ~ "Temperature (°C)",
    "phaseyes" ~ "Drone (present)",
    "periodbefore" ~ "Drone (after)",
    .default = term 
  )) |>
  rename(
    Model = model,
    `Explanatory variable` = term,
    `Estimate` = estimate,
    `Standard error` = std.error,
    `Z-value` = statistic,
    `P-value` = p.value
  ) |>  select(Model, `Explanatory variable`, Estimate, `Standard error`, `Z-value`, `P-value`) |> 
  flextable() |> 
  autofit() 

## 5.3. Distance travelled ----

summary_lm2e <- lm2e |> broom.mixed::tidy() |> mutate(model = "Day level")
summary_lm2f <- lm2f |> broom.mixed::tidy() |> mutate(model = "Hour level")

tra_sum <- bind_rows(summary_lm2e, summary_lm2f) |> 
  mutate(across(where(is.numeric), ~ round(., digits = 3))) |> 
  filter(effect == "fixed") |> 
  filter(term != "(Intercept)") |> 
  mutate(term = case_match(
    term,
    "bin_pyes" ~ "Rainfall (mm)",
    "mean_w" ~ "Wind speed (m/s)",
    "mean_t" ~ "Temperature (°C)",
    "phaseyes" ~ "Drone (present)",
    "periodbefore" ~ "Drone (after)",
    .default = term 
  )) |>
  rename(
    Model = model,
    `Explanatory variable` = term,
    `Estimate` = estimate,
    `Standard error` = std.error,
    `T-value` = statistic,
    `P-value` = p.value
  ) |>  select(Model, `Explanatory variable`, Estimate, `Standard error`, `T-value`, `P-value`) |> 
  flextable() |> 
  autofit() 

## 5.4. Space use ----

space_sum <- lm2g |> broom.mixed::tidy() |> 
  mutate(across(where(is.numeric), ~ round(., digits = 3))) |> 
  filter(effect == "fixed") |> 
  filter(term != "(Intercept)") |> 
  mutate(term = case_match(
    term,
    "bin_pyes" ~ "Rainfall (mm)",
    "mean_w" ~ "Wind speed (m/s)",
    "mean_t" ~ "Temperature (°C)",
    "phaseyes" ~ "Drone (present)",
    .default = term 
  )) |>
  rename(
    `Explanatory variable` = term,
    `Estimate` = estimate,
    `Standard error` = std.error,
    `T-value` = statistic,
    `P-value` = p.value
  ) |>  select(`Explanatory variable`, Estimate, `Standard error`, `T-value`, `P-value`) |> 
  flextable() |> 
  autofit() 

## 5.5. Combined ----

flextable::save_as_docx(
  "iid" = iid_sum,
  "flock" = flock_sum,
  "travel" = tra_sum,
  "space" = space_sum,
  path = "tables_stress.docx")
