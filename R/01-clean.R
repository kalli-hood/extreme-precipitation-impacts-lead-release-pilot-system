## This is the primary cleaning file.
# The details have been omitted for privacy, and cleaned files have been provided.
# ------------------------------------------------------- #
# library ------------------------------------------------
# ------------------------------------------------------- #
library(readxl)
library(tidyverse)
library(cwrshelpr)
library(janitor)
library(lubridate)

# ------------------------------------------------------- #
# setup --------------------------------------------------
# ------------------------------------------------------- #
# Define the flood date
flood_date <- as.Date("2023-07-24")

#path to all ICP-MS Files
files <- list.files(
  path = here::here("data/raw/pipe_loop"),
  full.names = TRUE,
  pattern = ".+\\.xlsx"
)

# series configuration 
materials <- tribble(
  ~replicate, ~material,
  "1",     "Cast Iron",
  "2",     "PVC",
  "3",     "Cast Iron",
  "4",     "PVC",
  "5",     "Cast Iron",
  "6",     "PVC",
)


# ------------------------------------------------------- #
# ---------------------- IMPORT RAW DATA ----------------
# ------------------------------------------------------- #

# ICP-MS Detection limits.
mdls <- read_xlsx("data/raw/2023-10-13_ICPMS_Detection_Limits.xlsx") |>
  clean_names() |>
  mutate(element = str_replace_all(element, pattern = "[:digit:]", ""),
         detection_limit_ppb = as.numeric(detection_limit_ppb))

# Parse ICP-MS Files
data_raw <- files %>%
  set_names() %>%
  map_dfr(read_icp, .id = "file") %>%
  left_join(mdls, by = "element")

# Read in Online Analyzer Organic metrics
uv_toc_raw <- read.csv("data/raw/waterquality/JDK_organics.csv") %>%
  clean_names()

# Read in Historical UV Data
uv_historical <- read.csv("data/raw/waterquality/HW_PW_UV254_historical.csv") |>
  clean_names()

# Read in TOC Grab
toc_grab_raw <- read_csv("data/raw/waterquality/TOCgrab2019-2024.csv")

## 2024 update
uv_new <- read_csv("data/raw/waterquality/JDK-uv-Mar-Dec-2024.csv") %>%
  rename(time = date,
         Finished = fin_uv,
         Raw = raw_uv) %>%
  pivot_longer(cols = Raw:Finished, 
               names_to = "location", values_to = "value") %>%
  mutate(
    time = as.Date(time, format = "%m/%d/%Y"),
  ) %>%
  select(time, value, location)

# TOC Grab, new
toc_new <- read_csv("data/raw/waterquality/JDK-fin-TOC-Mar-Dec-2024.csv") 


# ------------------------------------------------------- #
# Clean Raw ICPMS files ---------------------------------
# ------------------------------------------------------- #

data_icpms <- data_raw |>
  filter(
    estimate_type == "Concentration average",
    str_detect(sample_name, "KH_") == TRUE, 
    str_detect(sample_name, "PW-CW") == FALSE, ## remove CW
    str_detect(sample_name, "C\\d+") == FALSE, #remove PACl cells
    str_detect(sample_name, "CC") == FALSE, # remove kh cells
    str_detect(sample_name, "Bottle") == FALSE, # remove kh cells mislabelled 
    str_detect(sample_name, "\\?") == FALSE,
    unit != "%"
  ) |>
  select(-file, -estimate_type, -isotope) |>
  mutate(
    sample_name = str_to_lower(sample_name),
    sample_name = str_remove(sample_name, "kh_"),
    sample_name = str_remove(sample_name, "url:"),
    date = ymd(str_extract(sample_name, "\\d+-\\d+-\\d+")),
    code = str_sub(sample_name, start = 12),
    code = ifelse(str_detect(code, "blank") == TRUE, "blank", code),
   flag = ifelse(str_detect(sample_name, "_?") == TRUE, 1, 0),
   rerun = ifelse(str_detect(sample_name, "rerun") == TRUE, 1, 0),
   l_cens = ifelse(value < detection_limit_ppb, TRUE, FALSE),
   value_cens = ifelse(l_cens == TRUE, detection_limit_ppb/sqrt(2), value),
    pipe = str_extract(code, "pb|cu"),
    pipe = ifelse(pipe == "cu", "cu-pb", pipe),
    dilution_code = str_extract_all(code, "_\\d+x|tot|0.45f", simplify = TRUE),
    sample_type = case_when(
      dilution_code[,1] == "0.45f" ~ "Filtered, <0.45um",
      dilution_code[,2] == "5x" ~ "Filtered, <0.45um",
      # some instances where total were filtered at 5x, 
      ## this ordering will change it appropriately
      dilution_code[,1] == "tot" ~ "Total",
      TRUE ~ "Total"
    ),
   replicate = str_remove_all(code, "_pb|pb|_cu|cu|_\\d+x|_10|tot|0.45f|pl|pl_|rerun| |_"),
    series = ifelse(is.na(replicate) == TRUE, "blank",
                    paste0(pipe, replicate, sep = "")),
    main = case_when(
      replicate %in% c('1', '3', '5') ~ "CI",
      TRUE ~ "PVC"
    ),
    year = year(date),
    month = month(date),
    week = isoweek(date),
    yday = yday(date),
    date_numeric = as.numeric(date)
   # sample_type = factor(ifelse(str_detect(sample_name, "5x"), "Filtered, <0.45um", "Total")),
  ) %>%
  group_by(pick(-value, -value_cens, -rerun)) %>%
  mutate(has_nonrerun = any(rerun == 0, na.rm = TRUE)) %>%
  filter(!(has_nonrerun & rerun == 1)) %>%
  ungroup() %>%
  select(
    date, date_numeric, year, month, week, yday,
    sample_name, code,
    pipe, main, replicate, series,
    element, value, 
    value_cens, l_cens,
    sample_type,
    unit
  ) %>%
  mutate(
    days_since_flood = date_numeric - as.numeric(as.Date("2023-07-24")),
    days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood)
  )

# ARCHIVE: Rerun Checker --------------------------------------------------
# 
# 
# data_icpms_reruns <- data_icpms %>%
#   filter(rerun == 1) %>%
#   select(date, series, element, value, sample_type, l_cens) %>%
#   rename(rerun_value = value,
#          rerun_cens = l_cens)
# 
# data_icpms_no_rerun <- data_icpms %>%
#   filter(rerun == 0) %>%
#   group_by(date, code, element, sample_type) %>%
#   nest()
#
# 
# data_icpms_rerun_check <- data_icpms_reruns %>%
#   left_join(data_icpms_no_rerun, by = c("date", "series", "element", "sample_type")) %>%
#   select(date, series, element, sample_type, value, l_cens, value_cens, rerun_value, rerun_cens) %>%
#   mutate(per_err = abs((rerun_value - value)/(rerun_value + value)),
#          flag = ifelse(per_err > 0.01, 1, 0),
#          cens = l_cens + rerun_cens)



# -------------------------------------------------------------------------

blanks <- data_icpms %>%
  filter(code == "blank")

data_icpms <- data_icpms %>%
  filter(code != "blank") 

# average duplicates 
data2 <- data_icpms |>
  group_by(date, date_numeric, year, month, week, yday, days_since_flood, 
           sample_name, pipe, main, replicate, series, 
           sample_type, unit, element) %>%
  summarise(value = mean(value),
            value_cens = mean(value_cens),
            l_cens = mean(l_cens)) %>%
  ungroup() %>%
  filter(series %in% c(
    "pb1", "cu-pb1",
    "pb2", "cu-pb2",
    "pb3", "cu-pb3",
    "pb4", "cu-pb4",
    "pb5", "cu-pb5",
    "pb6", "cu-pb6"
  ),
  value < 1000)


data_clean <- data2 |>
  left_join(materials, by = "replicate") #|>
 # mutate(series = paste(series, pipe, sep = "_")) |>
  # select(date, series, pipe, material, element, value, l_cens, sample_type) #|>
  # mutate(
  #   year = year(date),
  #   week_yr = isoweek(date),
  #   week = week_yr + 52*(year - 2021)
  # )


## restrict analysis window 
# flood = July 24, 2023



# org - online analyzer ---------------------------------------------------
uv_toc_oa <- uv_toc_raw %>%
  select(-x) %>%
  pivot_wider(
    id_cols = c("date", "location"),
    names_from = parameter, 
    values_from = value
  ) %>%
  clean_names() |>
  transmute(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    week = isoweek(date),
    yday = yday(date),
    date_numeric = as.numeric(date),
    post_flood = ifelse(date > as.Date("2023-07-24"), 1, 0),
    days_since_flood = date_numeric - as.numeric(as.Date("2023-07-24")),
    days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood),
    location, uv254, toc, suva
  )

# uv historical -----------------------------------------------------------
 uv_finished_hist <- uv_historical %>%
  mutate(time = as.Date(time)) %>%
  bind_rows(uv_new) %>%
  rename(date = time) %>%
  mutate(year = year(date),
         month = month(date),
         week = isoweek(date),
         yday = yday(date),
         date_numeric = as.numeric(date),
         post_flood = ifelse(date < flood_date, 0, 1),
         days_since_flood = date_numeric - as.numeric(as.Date("2023-07-24")),
         days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood)) %>%
  filter(
    value < 0.28 # remove error values
    ) %>%
  arrange(date) %>%
  rename(uv = value) %>%
  group_by(pick(-c(uv))) %>%
  summarize(uv = mean(uv)) 


# TOC grab ----------------------------------------------------------------
toc_grab <- toc_grab_raw %>%
  select(date2, sample_type, result) %>%
  rename(date = date2) %>%
  clean_names()  %>%
  select(date, result, sample_type)

toc_grab2 <- toc_new %>%
  clean_names() %>%
  select(date, result) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    sample_type = "Finished"
  ) 

toc_grab_clean <- toc_grab %>%
  bind_rows(toc_grab2) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    week = isoweek(date),
    yday = yday(date),
    date_numeric = as.numeric(date),
    post_flood = ifelse(date > as.Date("2023-07-24"), 1, 0),
       days_since_flood = date_numeric - as.numeric(as.Date("2023-07-24")),
       days_since_flood = ifelse(days_since_flood < 0, 0, days_since_flood)) %>%
  group_by(pick(-c(result))) %>%
  summarize(
    toc = mean(result)
  ) %>%
  ungroup()


# merge datasets ----------------------------------------------------------

data_pl_uv_toc <- data_clean %>%
  left_join(uv_toc_oa, by = c("date", "year", "month", "week", "yday", "date_numeric"))

data_pl_uv_fin_historical <- data_clean %>%
  left_join(uv_finished_hist, by = c("date", "year", "month", "week", "yday", "date_numeric"))

pb_clean <- data_clean |>
  filter(element == "Pb")

pb_uv_toc <- data_pl_uv_toc %>%
  filter(element == "Pb")

pb_uv_uv_fin_hist <- data_pl_uv_fin_historical %>%
  filter(element == "Pb")

# write -------------------------------------------------------------------

# Metals & Pb only
write_rds(data_clean, "data/clean/rds/pl_metals-2022-2024.rds")
write_rds(pb_clean, "data/clean/rds/pl-pb-2022-2024.rds")

# Metals / Pb with UV+TOC from OA
write_rds(data_pl_uv_toc, "data/clean/rds/pl_metals_uv_toc_OA-2022-2024.rds")
write_rds(pb_uv_toc, "data/clean/rds/pl_pb_uv_toc_OA-2022-2024.rds")

# Online Analyzer UV-TOC-SUVA
write_rds(uv_toc_oa, "data/clean/rds/online-analyzer-uv-toc-2023-2024.RDS")

# UV Historical 
write_rds(uv_finished_hist, "data/clean/rds/uv-finished-2009-2024-clean.RDS")

write_rds(toc_grab_clean, "data/clean/rds/GRAB-toc-fin-2019-2024.RDS")

### CSV ----
# Metals & Pb only
write_csv(data_clean, "data/clean/csv/pl_metals-2022-2024.rds")
write_csv(pb_clean, "data/clean/csv/pl-pb-2022-2024.rds")

# Metals / Pb with UV+TOC from OA
write_csv(data_pl_uv_toc, "data/clean/csv/pl_metals_uv_toc_OA-2022-2024.rds")
write_csv(pb_uv_toc, "data/clean/csv/pl_pb_uv_toc_OA-2022-2024.rds")

# Online Analyzer UV-TOC-SUVA
write_csv(uv_toc_oa, "data/clean/csv/online-analyzer-uv-toc-2023-2024.RDS")

# UV Historical 
write_csv(uv_finished_hist, "data/clean/csv/uv-finished-2009-2024-clean.RDS")

write_csv(toc_grab_clean, "data/clean/csv/GRAB-toc-fin-2019-2024.RDS")

