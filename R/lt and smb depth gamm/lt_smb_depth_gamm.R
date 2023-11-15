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

dat <- read_rds(here("Data",
                     "Daily SMB Depth Data", 
                     "daily_smb_depth_data.rds"))


glimpse(data)
# ---- look at the distribution ---- 

ggplot(data = dat, aes(x = mean_value)) + 
  geom_histogram()


# ---- create gam models -----

m <- bam(mean_value ~ species +
           ti(doy, fish_basin, by = species, bs = c("cc", "fs"), 
             k = c(15, 3)) + 
           s(doy, by = species, k = 15, bs = "cc") +
           s(floy_tag, year, bs = c("re", "re")),
         method = "fREML", 
         data = dat, 
         family = Gamma(link = "log"), 
         select = TRUE)

r1 <- start_value_rho(m, plot = TRUE)

r1
m1 <- bam(
  mean_value ~ species +
    ti(doy, fish_basin, by = species, bs = c("cc", "fs"), 
       k = c(15, 3)) + 
    s(doy, by = species, k = 15, bs = "cc") +
    s(floy_tag, year, bs = c("re", "re")),
  method = "fREML", 
  data = dat, 
  family = Gamma(link = "log"), 
  select = TRUE, 
  discrete = TRUE, 
  rho = r1, 
  AR.start = start_event
)

gam.check(m)
gam.check(m1)
appraise(m)
appraise(m1)

write_rds(m1, here("model objects", 
                   "lt_smb_gamm_depth.rds"))


