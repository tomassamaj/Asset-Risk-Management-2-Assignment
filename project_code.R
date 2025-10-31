
all_packages <- c(
  "dplyr", "tidyr", "lubridate", "stringr",
  "rstudioapi",
  "frenchdata",
  "ggplot2", "scales", "corrplot", "RColorBrewer"
)

options(repos = "https://cloud.r-project.org")

installed <- rownames(installed.packages())
for(pkg in all_packages) {
  if(! pkg %in% installed) install.packages(pkg)
}

# load all necessary packages
invisible(lapply(all_packages, library, character.only = TRUE))

# get current wd
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())



load("data.RData")


############################ PART 1.  ##########################################
# --- 1. Load and Prepare Factor/Theme Data ---

# Define start and end dates for analysis
start_date <- ymd("1926-01-01") # Match factor data availability 
end_date <- ymd("2015-12-31")   # Match factor data availability 



# --- 2. Load and Prepare Fama-French Data ---

factors_ff3_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")

factors_ff3_daily <- factors_ff3_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |>
  filter(date >= start_date & date <= end_date)




# For each month t, compute the realized variance using all daily returns within
# that month. Note that the number of trading days per month is not always the
# same

factors_ff3_monthly <- factors_ff3_daily |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(year, month) |>
  summarize(
    date = max(date), # Use end-of-month date
    
    # CRITICAL: Calculate the sum of squared demeaned returns, not the sample variance.
    # This matches the RV definition in Moreira & Muir (2017), Formula (2).
    mkt_excess_var = sum((mkt_excess - mean(mkt_excess))^2),
    smb_var = sum((smb - mean(smb))^2),
    hml_var = sum((hml - mean(hml))^2),

    n_days = n(), # Good to keep for verification
    .groups = "drop"
  ) |>
  arrange(date) |>
  # You only need the market variance for Part 1.
  # I've removed smb_var and hml_var to focus on Part 1.
  select(date, mkt_excess_var, smb_var, hml_var, n_days)



















############################ PART2.  ##########################################
# --- 1. Load and Prepare Factor/Theme Data ---

# Define start and end dates for analysis
start_date_part_2 <- ymd("1926-01-01") # Match factor data availability 
end_date_part_2 <- ymd("2025-07-31")   # Match factor data availability 



# --- 2. Load and Prepare Fama-French Data ---

factors_ff3_daily_raw_part_2 <- download_french_data("Fama/French 3 Factors [Daily]")

factors_ff3_daily_part_2 <- factors_ff3_daily_raw_part_2$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |>
  filter(date >= start_date & date <= end_date)




# For each month t, compute the realized variance using all daily returns within
# that month. Note that the number of trading days per month is not always the
# same

factors_ff3_monthly_part_2 <- factors_ff3_daily_part_2 |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(year, month) |>
  summarize(
    date = max(date),
    mkt_excess_var = var(mkt_excess),
    smb_var = var(smb),
    hml_var = var(hml),
    .groups = "drop"
  ) |>
  arrange(date) |>
  select(date, mkt_excess_var, smb_var, hml_var)




# Load theme returns from CSV
#all_themes_monthly_vw_cap <- read.csv("[all_countries]_[all_themes]_[monthly]_[vw_cap].csv")

# types of locations in location column in the data:
unique(all_themes_monthly_vw_cap$location)


all_themes_monthly_vw_cap_wide_usa <- all_themes_monthly_vw_cap %>%
  mutate(date = ymd(date)) %>%
  filter(
    location == "usa",
    date >= start_date_part_2 & date <= end_date_part_2
  ) %>%
  select(date, name, ret) %>%
  pivot_wider(
    id_cols = date,
    names_from = name,
    values_from = ret,
    names_prefix = "ret_usa_"
  ) %>%
  arrange(date)



# Load theme returns from CSV
#all_themes_daily_vw_cap <- read.csv("[all_countries]_[all_themes]_[daily]_[vw_cap].csv")

# types of locations in location column in the data:
unique(all_themes_daily_vw_cap$location)


all_themes_daily_vw_cap_wide_usa <- all_themes_daily_vw_cap %>%
  mutate(date = ymd(date)) %>%
  filter(
    location == "usa",
    date >= start_date_part_2 & date <= end_date_part_2
  ) %>%
  select(date, name, ret) %>%
  pivot_wider(
    id_cols = date,
    names_from = name,
    values_from = ret,
    names_prefix = "ret_usa_"
  ) %>%
  arrange(date)






