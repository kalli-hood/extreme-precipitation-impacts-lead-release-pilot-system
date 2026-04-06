## This script takes the cleaned data and prepares the 
## datasets for GAMM modelling. The model-in files are provided in
## data/model-in 

## This script does not need to be run by the user if they have the model in files.

# libraries ---------------------------------------------------------------

library(broom)
library(tidyverse)
library(janitor)
library(mgcv)
library(gratia)
library(paletteer)
library(wesanderson)

# Setup -------------------------------------------------------------------
# select whichever appropriate.
rds_dir = "data/clean/rds/"
#csv_dir = "data/clean/csv/"

suffix = ".rds"
#suffix = ".csv"

path_uvf_hist = "uv-finished-2009-2024-clean"
path_toc_grab = "GRAB-toc-fin-2019-2024"
path_org_OA = "online-analyzer-uv-toc-2023-2024"
path_metals = "pl_metals-2022-2024"


### DATES
flood_date <- as.Date("2023-07-24")
flood_date_num <- as.numeric(flood_date)

# Import data -------------------------------------------------------------

## GRAB SAMPLE DATA SETS
uv <- readRDS(paste0(rds_dir, path_uvf_hist, suffix)) 
toc <- readRDS(paste0(rds_dir, path_toc_grab, suffix))

## ONLINE ANALYZER DATA
org_oa <- readRDS(paste0(rds_dir, path_org_OA, suffix)) 

## ICP-MS DATA - PL
icpms <- readRDS(paste0(rds_dir, path_metals, suffix))

## optional, to clean up
#rm(list = setdiff(ls(), c("icpms", "org_oa", "toc", "uv")))

# data prep ---------------------------------------------------------------

icpms <- icpms %>%
  filter(l_cens == 0,
         year > 2022) %>%
  group_by(element, sample_type, pipe, 
           main, date, date_numeric, 
           yday, year, week, series) %>%
  summarize(value = median(value)) %>%
  ungroup() %>%
  mutate(series = factor(series),
         log_value = log(value),
         post_flood = ifelse(date > as.Date("2023-07-24"), 1, 0),
         lsl = case_when(
           pipe == "cu-pb" ~ 1,
           pipe == "pb" ~ -1
         ),
         dist_main = case_when(
           main == "CI" ~ 1,
           main == "PVC" ~ -1
         ),
         lsl = as.numeric(lsl),
         dist_main = as.numeric(dist_main),
         days = date_numeric - flood_date_num,
         days_since_flood = date_numeric - as.numeric(as.Date("2023-07-24")),
         days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood)
  )


# prep model data ---------------------------------------------------------
toc <- toc %>%
  mutate(
    days = date_numeric - flood_date_num
  )

org_oa <- org_oa %>%
  mutate(
    days = date_numeric - flood_date_num
  )

model_in_org_oa_f <- org_oa %>%
  filter(location == "Finished")
model_in_org_oa_r <- org_oa %>%
  filter(location == "Raw")

uv <- uv %>%
  filter(uv > 0)

model_in_uv_raw <- uv %>%
  filter(location == "Raw")
model_in_uv_fin <- uv %>%
  filter(location == "Finished")


model_in_fe <- icpms %>%
  filter(element == "Fe",
         sample_type == "Total")
model_in_cu <- icpms %>%
  filter(element == "Cu",
         sample_type == "Total")
model_in_pb <- icpms %>%
  filter(element == "Pb",
         sample_type == "Total")
model_in_al <- icpms %>%
  filter(element == "Al",
         sample_type == "Total")
model_in_mn <- icpms %>%
  filter(element == "Mn",
         sample_type == "Total")
model_in_p <- icpms %>%
  filter(element == "P",
         sample_type == "Total") %>%
  mutate(sqrt_val = sqrt(value))


saveRDS(model_in_pb, "data/model-in/model_in_pb.rds")
saveRDS(model_in_fe, "data/model-in/model_in_fe.rds")
saveRDS(model_in_cu, "data/model-in/model_in_cu.rds")
saveRDS(model_in_al, "data/model-in/model_in_al.rds")
saveRDS(model_in_mn, "data/model-in/model_in_mn.rds")
saveRDS(model_in_p, "data/model-in/model_in_p.rds")
saveRDS(model_in_uv_fin, "data/model-in/model_in_uvf.rds")
saveRDS(model_in_uv_raw, "data/model-in/model_in_uvr.rds")
saveRDS(toc, "data/model-in/model_in_toc_grab_f.rds")
saveRDS(model_in_org_oa_r, "data/model-in/model_in_org_oa_r.rds")
saveRDS(model_in_org_oa_f, "data/model-in/model_in_org_oa_f.rds")



