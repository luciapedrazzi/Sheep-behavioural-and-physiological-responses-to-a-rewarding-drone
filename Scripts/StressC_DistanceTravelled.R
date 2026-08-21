############# DISTANCE TRAVELLED ----

# 0. Weather df ----

weather_day <- read.csv("C:/Users/2320751/OneDrive - Swansea University/#Running/Stress chapter/Clean_Submission/weather_day.csv")

weather_day <- weather_day |>
  mutate(
    date = as.Date(date)
  )

weather_period <- read.csv("C:/Users/2320751/OneDrive - Swansea University/#Running/Stress chapter/Clean_Submission/weather_period.csv")

weather_period <- weather_period |>
  mutate(
    date = as.Date(date)
  )

# 1. Day level (Phase) ----
# smoothed_filtered can be created by running the filtering steps used for newdata_filtered

smoothed_filtered <- read.csv("C:/Users/2320751/OneDrive - Swansea University/#Running/Stress chapter/Clean_Submission/smoothed_filtered.csv")

smoothed_filtered <- smoothed_filtered |>
  mutate(
    timestamp = as.POSIXct(timestamp),
    date = as.Date(date)
  )

travel_day <- smoothed_filtered |>
  group_by(TagID, date) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    dist_raw = c(0, geodist(cbind(X, Y), 
                            measure = "geodesic", 
                            sequential = TRUE)),
    time_diff = c(0, as.numeric(diff(timestamp), units = "secs")),
    dist_cleaned = ifelse(dist_raw < 0.2 | time_diff > 10, 0, dist_raw)
  ) |>
  summarise(
    total_d = sum(dist_cleaned, na.rm = TRUE),
    travel_rate_d = total_d / n(),
    tra_adj = travel_rate_d * 36000,
    .groups = "drop"
  ) 

df_tra_d <- right_join(weather_day, travel_day, by="date") |>
  mutate(
    phase = case_when(
      date <= "2024-06-30" | date=="2024-07-16"
      | date=="2024-07-18" | date=="2024-07-20"
      | date=="2024-07-21" ~ "no",
      date >= "2024-07-01" & date <= "2024-07-15" 
      | date=="2024-07-17" | date=="2024-07-19"
      | date=="2024-07-22" ~ "yes"
    )
  ) 

write.csv (df_tra_d, "df_tra_d.csv", row.names = F)

# 2. Hour level (Period) ----
# smoothed_period can be created by running the filtering steps used for newdata_period

smoothed_period <- read.csv("C:/Users/2320751/OneDrive - Swansea University/#Running/Stress chapter/Clean_Submission/smoothed_period.csv")

smoothed_period <- smoothed_period |>
  mutate(
    timestamp = as.POSIXct(timestamp),
    date = as.Date(date)
  )

travel_h <- smoothed_period |>
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC")) |>
  group_by(TagID, date, period) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    dist_raw = c(0, geodist(cbind(X, Y), 
                                measure = "geodesic", 
                                sequential = TRUE)),
    time_diff = c(0, as.numeric(diff(timestamp), units = "secs")),
    dist_cleaned = ifelse(dist_raw < 0.2 | time_diff > 10, 0, dist_raw_smo)
  ) |>
  summarise(
    total_m = sum(dist_cleaned, na.rm = TRUE),
    travel_rate = total_m / n(),
    travel_adj = travel_rate * 3600,
    .groups = "drop"
  )

df_tra_h <- merge(weather_period |> mutate (date=as.Date(date)), travel_h, by=c("date","period"), keep.all=T)

write.csv(df_tra_h, "df_tra_h.csv", row.names = F)
