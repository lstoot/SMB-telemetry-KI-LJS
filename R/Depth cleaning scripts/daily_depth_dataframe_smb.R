# --- bring in packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(gratia)
  library(here)
  library(itsadug)
  library(mgcv)
  library(readr)
}

# ---- bring in data ----- 

smb <- read_rds(here("Data",
                     "Cleaned detection data",
                     "smb_telemetry_data.rds"))

glimpse(smb)

# ---- create daily summary dataframe grouped by day of year -----
smb_sum <- smb %>% 
  filter(passed_filter == 1) %>% 
  group_by(
    species, floy_tag, doy, sensor_unit, fish_basin, month, season, year) %>% 
  summarise(
    mean_value = mean(sensor_value), 
    # n_tag = n_distinct(floy_tag)
  ) %>% 
  ungroup()


# --- subset out just depth from summary dataframe 
smb_depth <- smb_sum %>% 
  filter(sensor_unit == "m" & mean_value > 0)

glimpse(smb_depth)


# ---- create starting point for model to run ----
# we will use the minmum day of year as the starting point --- 
smb_depth <- smb_depth %>% 
  group_by(floy_tag) %>% 
  arrange(floy_tag, year, doy) %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE), 
         floy_tag = factor(floy_tag), 
         species = factor(species), 
         year = factor(year)) %>% 
  ungroup() %>% 
  arrange(doy, start_event)


write_rds(smb_depth, here::here("Data", 
                          "Daily SMB Depth Data", 
                          "daily_smb_depth_data.rds"))
