
all_packages <- c(
  "dplyr", "tidyr", "lubridate", "stringr",
  "rstudioapi",
  "frenchdata",
  "ggplot2", "scales", "corrplot", "RColorBrewer","tidyquant", "readr"
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


### STEP 1 ###

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


### STEP 2 ### 

# --- Construct the volatility-managed portfolio ---
#
# The formula is: f_managed(t+1) = c * f_original(t+1) / var(t)
#
# We will:
#  1. Calculate the original monthly excess returns (f_original).
#  2. Combine them with the lagged variance (var(t)) from your Step 1.
#  3. Calculate the scaling constant 'c'.
#  4. Calculate the final managed portfolio return (f_managed).

# --- 2a. Calculate Original Monthly Excess Returns (f_original) ---

# We need the original monthly excess return, f(t+1).
# This must be calculated by compounding the daily returns from factors_ff3_daily.
# Monthly excess return = (Compounded Mkt Return) - (Compounded RF Return)

original_monthly_returns <- factors_ff3_daily |>
  mutate(
    year = year(date),
    month = month(date),
    mkt_return_daily = mkt_excess + rf # Daily total market return
  ) |>
  group_by(year, month) |>
  summarize(
    date = max(date), # Use end-of-month date
    
    # Compound market return
    mkt_return_comp = prod(1 + mkt_return_daily) - 1,
    
    # Compound risk-free rate
    rf_comp = prod(1 + rf) - 1,
    
    # The "original monthly excess return"
    mkt_excess_orig = mkt_return_comp - rf_comp,
    .groups = "drop"
  ) |>
  select(date, mkt_excess_orig)


# --- 2b. Combine Returns and Lagged Variance ---

# Join the original returns with the variance calculated in Step 1
# (factors_ff3_monthly already exists from your code)
data_monthly <- factors_ff3_monthly |>
  left_join(original_monthly_returns, by = "date") |>
  
  # The formula uses variance from month *t* [var(t)] to scale the 
  # return from month *t+1* [f_original(t+1)].
  # We lag mkt_excess_var to get var(t) aligned with f(t+1).
  mutate(
    mkt_excess_var_lag = lag(mkt_excess_var)
  ) |>
  
  # We can't calculate a managed return for the very first month,
  # as we have no prior variance.
  filter(!is.na(mkt_excess_var_lag)) |>
  
  # Re-order for clarity
  select(date, mkt_excess_orig, mkt_excess_var_lag, everything())


# --- 2c. Calculate the Scaling Constant 'c' ---

# First, create the unscaled managed portfolio (where c=1)
data_monthly <- data_monthly |>
  mutate(
    # This is f_original(t+1) / var(t)
    mkt_excess_unscaled = mkt_excess_orig / mkt_excess_var_lag
  )

# Next, find 'c' by matching the standard deviations
sd_orig <- sd(data_monthly$mkt_excess_orig, na.rm = TRUE)
sd_unscaled <- sd(data_monthly$mkt_excess_unscaled, na.rm = TRUE)

# Calculate the scaling constant 'c'
c_scalar <- sd_orig / sd_unscaled


# --- 2d. Calculate the Final Managed Portfolio Return ---

# Apply the scaling constant 'c' to the unscaled portfolio
factors_vol_managed <- data_monthly |>
  mutate(
    mkt_excess_managed = c_scalar * mkt_excess_unscaled
  )


# --- 3. Verification ---

# Print the constant 'c'
print(paste("Scaling constant 'c':", round(c_scalar, 6)))

# Check the standard deviations (they should be identical)
sd_managed <- sd(factors_vol_managed$mkt_excess_managed, na.rm = TRUE)
print(paste("Original Portfolio SD:", round(sd_orig, 6)))
print(paste("Managed Portfolio SD: ", round(sd_managed, 6)))

# View the head of the final monthly data
print(head(factors_vol_managed))


### STEP 3 ###
# --- Sort months into variance quintiles ---
#
# Sort months into 5 groups based on lagged variance (mkt_excess_var_lag).
# Label the groups from "Low Vol" (lowest variance) to "High Vol" 
# (highest variance).

factors_vol_managed <- factors_vol_managed |>
  mutate(
    # Create 5 quintiles based on the lagged variance.
    # ntile() creates 5 groups of roughly equal size.
    var_quintile = ntile(mkt_excess_var_lag, 5),
    
    # Convert the numeric quintile (1, 2, 3, 4, 5) to a factor with clear labels
    var_quintile_labeled = factor(
      var_quintile,
      levels = 1:5,
      labels = c("Low Vol", "Q2", "Q3", "Q4", "High Vol"),
      ordered = TRUE # Mark this as an ordered factor
    )
  )


# --- 4. Verification ---

# Show the head of the data with the new columns
print(head(factors_vol_managed |> 
             select(date, mkt_excess_var_lag, var_quintile, var_quintile_labeled)))

# Show a table with the counts in each quintile to ensure they are balanced
print("Count of months in each variance quintile:")
print(table(factors_vol_managed$var_quintile_labeled))

### STEP 4 ###
# --- Reproduce the figures and tables ---

# --- 4.A: Figure 1 - Load NBER Recession Data ---
nber_data_raw <- read_csv("USREC.csv")

# Process the data
nber_data <- nber_data_raw %>%
  mutate(
    # FIX: Use the correct column name 'observation_date'
    date = ymd(observation_date), 
    
    # This data is already monthly, so floor_date is good but not strictly necessary
    date_join = floor_date(date, "month"), 
    
    us_recession = as.numeric(USREC) 
  ) %>%
  # The rest of your code was correct, but since the data is already monthly,
  # the group_by/summarize is redundant. We just select the columns.
  select(date_join, us_recession)

factors_vol_managed <- factors_vol_managed %>%
  mutate(
    date_join = floor_date(date, "month")
  ) %>%
  left_join(nber_data, by = "date_join") %>%
  select(-date_join)


# --- 4.B: Figure 1 - Summary Statistics ---

# Calculate all required statistics for the plots
quintile_summary <- factors_vol_managed |>
  group_by(var_quintile_labeled) |>
  summarize(
    # 1. Average monthly return (annualized)
    mean_ret_orig_ann = mean(mkt_excess_orig) * 12,
    mean_ret_man_ann = mean(mkt_excess_managed) * 12,
    
    # 2. Standard deviation of returns (annualized)
    sd_ret_orig_ann = sd(mkt_excess_orig) * sqrt(12),
    sd_ret_man_ann = sd(mkt_excess_managed) * sqrt(12),
    
    # 3. Ratio E[R]/Var(R) (using monthly data)
    mean_var_orig = mean(mkt_excess_orig) / var(mkt_excess_orig),
    mean_var_man = mean(mkt_excess_managed) / var(mkt_excess_managed),
    
    # 4. Probability of being in a recession
    prob_recession = mean(us_recession, na.rm = TRUE),
    
    .groups = "drop"
  )

# View summary table
print("--- Summary Statistics for Figure 1 ---")
print(quintile_summary)


# --- 4.C: Figure 1 - Create Bar Charts ---

# To plot Original vs. Managed side-by-side, we pivot the data to long format
# We do this separately for each plot to keep it clear

# Plot 1: Average Monthly Return (Annualized)
p1_data <- quintile_summary |>
  select(var_quintile_labeled, mean_ret_orig_ann, mean_ret_man_ann) |>
  pivot_longer(
    cols = -var_quintile_labeled,
    names_to = "portfolio",
    values_to = "return",
    names_prefix = "mean_ret_"
  ) |>
  mutate(portfolio = if_else(portfolio == "orig_ann", "Original", "Managed"))

p1 <- ggplot(p1_data, aes(x = var_quintile_labeled, y = return, fill = portfolio)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "1. Average Monthly Return (Annualized)",
    x = "Variance Quintile",
    y = "Annualized Return",
    fill = "Portfolio"
  ) +
  theme_minimal()

print(p1)

# Plot 2: Standard Deviation of Returns (Annualized)
p2_data <- quintile_summary |>
  select(var_quintile_labeled, sd_ret_orig_ann, sd_ret_man_ann) |>
  pivot_longer(
    cols = -var_quintile_labeled,
    names_to = "portfolio",
    values_to = "std_dev",
    names_prefix = "sd_ret_"
  ) |>
  mutate(portfolio = if_else(portfolio == "orig_ann", "Original", "Managed"))

p2 <- ggplot(p2_data, aes(x = var_quintile_labeled, y = std_dev, fill = portfolio)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "2. Standard Deviation of Returns (Annualized)",
    x = "Variance Quintile",
    y = "Annualized Std. Dev.",
    fill = "Portfolio"
  ) +
  theme_minimal()

print(p2)

# Plot 3: Ratio E[R]/Var(R)
p3_data <- quintile_summary |>
  select(var_quintile_labeled, mean_var_orig, mean_var_man) |>
  pivot_longer(
    cols = -var_quintile_labeled,
    names_to = "portfolio",
    values_to = "ratio",
    names_prefix = "mean_var_"
  ) |>
  mutate(portfolio = if_else(portfolio == "orig", "Original", "Managed"))

p3 <- ggplot(p3_data, aes(x = var_quintile_labeled, y = ratio, fill = portfolio)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title = "3. Ratio E[R] / Var(R)",
    x = "Variance Quintile",
    y = "Ratio (Monthly)",
    fill = "Portfolio"
  ) +
  theme_minimal()

print(p3)

# Plot 4: Probability of being in a Recession
# No pivot needed as it's a characteristic of the quintile
p4 <- ggplot(quintile_summary, aes(x = var_quintile_labeled, y = prob_recession)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "4. Probability of being in a Recession",
    x = "Variance Quintile",
    y = "Probability"
  ) +
  theme_minimal()

print(p4)


# --- 4.D: Table I (Panel A, Column 1) ---

# Run a regression of the managed portfolio on the original market return.
model_table1 <- lm(mkt_excess_managed ~ mkt_excess_orig, 
                   data = factors_vol_managed)

# Get the summary of the model
model_summary <- summary(model_table1)

# Report the required statistics
cat("\n--- Table I (Panel A, Column 1) ---\n")
cat(paste(
  "Intercept:",
  round(coef(model_table1)[1], 6),
  "\n"
))
cat(paste(
  "Slope (Beta):",
  round(coef(model_table1)[2], 6),
  "\n"
))
cat(paste(
  "R-squared:",
  round(model_summary$r.squared, 4),
  "\n"
))
cat(paste(
  "Number of observations:",
  nobs(model_table1),
  "\n"
))
cat(paste(
  "RMSE (Residual Std. Error):",
  round(model_summary$sigma, 6),
  "\n"
))










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






