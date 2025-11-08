# --- 1. Load All Necessary Packages ---

all_packages <- c(
  "dplyr", "tidyr", "lubridate", "stringr",
  "rstudioapi",
  "frenchdata",
  "ggplot2", "scales", "corrplot", "RColorBrewer",
  "tidyquant", "readr",
  "broom", # Added for tidy regression output
  "patchwork",
  "lmtest",
  "sandwich",
  "gt"
)

options(repos = "https://cloud.r-project.org")

installed <- rownames(installed.packages())
for(pkg in all_packages) {
  if(! pkg %in% installed) install.packages(pkg)
}

invisible(lapply(all_packages, library, character.only = TRUE))

# --- 2. Set Working Directory ---
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

############################ PART 1.  ##########################################

# --- 3. Load and Prepare Fama-French Data ---
start_date <- ymd("1926-01-01") 
end_date <- ymd("2015-12-31")   

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


### STEPS 1 & 2a (Combined): Calculate Monthly Returns and Variance ###
# Combine Step 1 (Variance) and Step 2a (Returns) into one operation
# to avoid grouping by month twice.

data_monthly <- factors_ff3_daily |>
  mutate(
    year_month = floor_date(date, "month"),
    mkt_return_daily = mkt_excess + rf
  ) |>
  group_by(year_month) |>
  summarize(
    date = max(date), # Use end-of-month date
    
    # --- From Step 1 ---
    mkt_excess_var = sum((mkt_excess - mean(mkt_excess))^2),
    smb_var = sum((smb - mean(smb))^2),
    hml_var = sum((hml - mean(hml))^2),
    n_days = n(),
    
    # --- From Step 2a ---
    mkt_return_comp = prod(1 + mkt_return_daily) - 1,
    rf_comp = prod(1 + rf) - 1,
    mkt_excess_orig = mkt_return_comp - rf_comp,
    
    .groups = "drop"
  ) |>
  select(-year_month) # Clean up grouping column


### STEPS 2b, 2c, 2d: Construct the Volatility-Managed Portfolio ###

# Create the lagged variance and unscaled return
data_unscaled <- data_monthly |>
  mutate(
    mkt_excess_var_lag = lag(mkt_excess_var)
  ) |>
  filter(!is.na(mkt_excess_var_lag)) |>
  mutate(
    mkt_excess_unscaled = mkt_excess_orig / mkt_excess_var_lag
  )

# Calculate the scaling constant 'c'
c_scalar <- sd(data_unscaled$mkt_excess_orig) / sd(data_unscaled$mkt_excess_unscaled)

# Apply the scaling
factors_vol_managed <- data_unscaled |>
  mutate(
    mkt_excess_managed = c_scalar * mkt_excess_unscaled
  )

# --- Verification ---
print(paste("Scaling constant 'c':", round(c_scalar, 6)))
print(paste("Original Portfolio SD:", round(sd(factors_vol_managed$mkt_excess_orig), 6)))
print(paste("Managed Portfolio SD: ", round(sd(factors_vol_managed$mkt_excess_managed), 6)))


### STEP 3: Sort Months into Variance Quintiles ###
factors_vol_managed <- factors_vol_managed |>
  mutate(
    var_quintile = ntile(mkt_excess_var_lag, 5),
    var_quintile_labeled = factor(
      var_quintile,
      levels = 1:5,
      labels = c("Low Vol", "Q2", "Q3", "Q4", "High Vol"),
      ordered = TRUE
    )
  )

# --- Verification ---
print("Count of months in each variance quintile:")
print(table(factors_vol_managed$var_quintile_labeled))


### STEP 4: Reproduce Figures and Tables ###

# --- 4.A: Load NBER Recession Data ---
nber_data_raw <- read_csv("USREC.csv")

nber_data <- nber_data_raw %>%
  mutate(
    date = ymd(observation_date), 
    date_join = floor_date(date, "month"), 
    us_recession = as.numeric(USREC) 
  ) %>%
  select(date_join, us_recession)

factors_vol_managed <- factors_vol_managed %>%
  mutate(
    date_join = floor_date(date, "month")
  ) %>%
  left_join(nber_data, by = "date_join") %>%
  select(-date_join)


# --- 4.B: Summary Statistics for Plots ---
quintile_summary <- factors_vol_managed |>
  group_by(var_quintile_labeled) |>
  summarize(
    mean_ret_orig_ann = mean(mkt_excess_orig) * 12,
    mean_ret_man_ann = mean(mkt_excess_managed) * 12,
    sd_ret_orig_ann = sd(mkt_excess_orig) * sqrt(12),
    sd_ret_man_ann = sd(mkt_excess_managed) * sqrt(12),
    mean_var_orig = mean(mkt_excess_orig) / var(mkt_excess_orig),
    mean_var_man = mean(mkt_excess_managed) / var(mkt_excess_managed),
    prob_recession = mean(us_recession, na.rm = TRUE),
    .groups = "drop"
  )

print("--- Summary Statistics for Figure 1 ---")
print(quintile_summary)


# --- 4.C: Create Bar Charts ---

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

# Plot 4: Probability of being in a Recession
p4 <- ggplot(quintile_summary, aes(x = var_quintile_labeled, y = prob_recession)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "4. Probability of being in a Recession",
    x = "Variance Quintile",
    y = "Probability"
  ) +
  theme_minimal()

# --- Combine all 4 plots using patchwork ---
# (p1 + p2) creates the top row
# (p3 + p4) creates the bottom row
# / combines the rows
# plot_layout(guides = "collect") creates a single shared legend

combined_plot <- (p1 + p2) / (p3 + p4) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") # Place shared legend at the bottom

# Print the combined plot
print(combined_plot)



# --- 4.D: Table I (Panel A, Column 1) ---
# 1. Run the model
model_table1 <- lm(mkt_excess_managed ~ mkt_excess_orig, 
                   data = factors_vol_managed)

# 2. Get robust coefficients and model statistics
model_coeffs_robust <- coeftest(model_table1, vcov = vcovHAC(model_table1))
model_stats <- broom::glance(model_table1)

# 3. Define the scaling factor
scaling_factor <- 1200 # Multiply by 12 for annualization and 100 for percentage

# 4. Create the final results table, scaling values "on-the-fly"
results_table <- tibble(
  Term = c("Alpha (α)", "MktRF (Slope)"),
  Estimate = c(
    model_coeffs_robust[1, "Estimate"] * scaling_factor, # Scale Alpha
    model_coeffs_robust[2, "Estimate"]                   # No scale for Slope
  ),
  StdError = c(
    model_coeffs_robust[1, "Std. Error"] * scaling_factor, # Scale Alpha SE
    model_coeffs_robust[2, "Std. Error"]                   # No scale for Slope SE
  )
)

# 5. Report all results in one table

# Get scaled values for the table
alpha_est <- model_coeffs_robust[1, "Estimate"] * scaling_factor
alpha_se <- model_coeffs_robust[1, "Std. Error"] * scaling_factor
slope_est <- model_coeffs_robust[2, "Estimate"]
slope_se <- model_coeffs_robust[2, "Std. Error"]
rmse <- model_stats$sigma * scaling_factor

# Create a single data.frame for all results
comparison_df <- tibble(
  Statistic = c("Alpha (α)", "MktRF (Slope)", "N", "R-squared", "RMSE"),
  Replicated = c(
    sprintf("%.2f (%.2f)", alpha_est, alpha_se),
    sprintf("%.2f (%.2f)", slope_est, slope_se),
    sprintf("%d", model_stats$nobs),
    sprintf("%.2f", model_stats$r.squared),
    sprintf("%.2f", rmse)
  ),
  Paper = c(
    "4.86 (1.56)",
    "0.61 (0.05)",
    "1065",
    "0.37",
    "51.39"
  )
)

# Build and print the 'gt' table
comparison_table <- comparison_df %>%
  gt() %>%
  tab_header(
    title = "Table 1 (Panel A, Col 1) Replication",
    subtitle = "Comparison of Replicated Results vs. Moreira & Muir (2017)"
  ) %>%
  cols_label(
    Statistic = "Statistic",
    Replicated = "Replicated Results",
    Paper = "Paper Results"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Replicated, Paper)
  ) %>%
  cols_align(
    align = "left",
    columns = Statistic
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_source_note(
    source_note = "Standard errors are in parentheses. Replicated SEs are Heteroskedasticity-Adjusted (HAC)."
  ) %>%

  # --- FIX ---
  # Apply border styling to the source note cell
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(2)
    ),
    locations = cells_source_notes()
  ) %>%
  # --- END FIX ---
  
  opt_table_lines("none") %>% # Clean look
  opt_table_outline(style = "solid", width = px(2)) %>%
  
  # --- Corrected tab_options() ---
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black"
    # The source_notes... arguments have been removed
  )

# Print the final table
print(comparison_table)










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













library(tidyquant)
library(zoo)
library(lubridate)
library(dplyr)
library(tidyverse)


# --- Part 2: Setup ---
# 1. Define your parameters
# !! Replace with your own student ID digits !!
digit_7 <- 2
digit_8 <- 6
D <- digit_7 + digit_8 + 10
print(paste("Using D =", D))

FACTOR_NAME <- "Momentum" # !! Replace with your chosen factor !!
FACTOR_COL_NAME <- paste0("ret_usa_", FACTOR_NAME)

# 2. Update end date and load full Fama-French daily data
start_date_ext <- ymd("1926-07-01")
end_date_ext <- ymd("2025-07-31") # As per assignment 

# (Assuming 'factors_ff3_daily_raw' is already loaded)
mkt_daily_ext <- factors_ff3_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    mkt_excess = as.numeric(`Mkt-RF`) / 100
  ) |>
  select(date, mkt_excess) |>
  filter(date >= start_date_ext & date <= end_date_ext)

# (Assuming 'all_themes_daily_vw_cap_wide_usa' is loaded and has the factor)
# Ensure the column name exists
if (!FACTOR_COL_NAME %in% names(all_themes_daily_vw_cap_wide_usa)) {
  stop(paste("Factor column", FACTOR_COL_NAME, "not found. Did you pick a valid factor?"))
}

factor_daily_ext <- all_themes_daily_vw_cap_wide_usa |>
  select(date, all_of(FACTOR_COL_NAME)) |>
  rename(factor_excess = !!FACTOR_COL_NAME) |>
  filter(date >= start_date_ext & date <= end_date_ext)

# 3. Join Mkt and Factor daily data
daily_data <- inner_join(mkt_daily_ext, factor_daily_ext, by = "date")

# --- Part 2: Step 1 ---
# 4. Calculate D-day rolling variance
# We use the tidyquant::tq_mutate wrapper for zoo::rollapply
# The formula is: (D/22) * variance
# Note: var(x) = sum((x - mean(x))^2) / (n-1). The assignment formula is sum(...),
# which is (n-1)*var(x). So we calculate rolling variance and multiply by (D-1)*(D/22).
# Let's re-read the formula :
# sigma_t^2 = (D/22) * SUM( (f_d - f_bar)^2 ) from d=1 to D
# The sum part is (D-1) * var(f).
# So the formula is: (D/22) * (D-1) * roll_var(f, D)

daily_data_with_var <- daily_data |>
  tq_mutate(
    select = mkt_excess,
    mutate_fun = rollapply,
    width = D,
    FUN = var, # Calculate rolling variance
    align = "right",
    fill = NA,
    col_rename = "mkt_roll_var_base"
  ) |>
  tq_mutate(
    select = factor_excess,
    mutate_fun = rollapply,
    width = D,
    FUN = var,
    align = "right",
    fill = NA,
    col_rename = "factor_roll_var_base"
  ) |>
  mutate(
    # Apply the assignment's scaling formula 
    mkt_d_day_var = (D / 22) * (D - 1) * mkt_roll_var_base,
    factor_d_day_var = (D / 22) * (D - 1) * factor_roll_var_base
  ) |>
  filter(!is.na(mkt_d_day_var)) # Remove initial NA rows

# 5. Get end-of-month variance to match with next month's return
variance_monthly_lag <- daily_data_with_var |>
  mutate(year_month = floor_date(date, "month")) |>
  group_by(year_month) |>
  summarize(
    # Get the *last* D-day variance from that month
    mkt_var_lag = last(mkt_d_day_var),
    factor_var_lag = last(factor_d_day_var)
  ) |>
  mutate(
    # This variance from month 't' will be used for month 't+1' returns
    date = year_month + months(1)
  ) |>
  select(date, mkt_var_lag, factor_var_lag)







# 1. Load MONTHLY Mkt returns (from daily FF data)
mkt_monthly_ext <- mkt_daily_ext |>
  mutate(
    rf = factors_ff3_daily$rf[match(date, factors_ff3_daily$date)], # Get matching RF
    mkt_return = mkt_excess + rf
  ) |>
  group_by(year_month = floor_date(date, "month")) |>
  summarize(
    date = max(date),
    mkt_return_comp = prod(1 + mkt_return) - 1,
    rf_comp = prod(1 + rf) - 1,
    mkt_excess_orig = mkt_return_comp - rf_comp
  ) |>
  select(date, mkt_excess_orig)

# 2. Load MONTHLY Factor returns
# (Assuming 'all_themes_monthly_vw_cap_wide_usa' is loaded)
factor_monthly_ext <- all_themes_monthly_vw_cap_wide_usa |>
  select(date, all_of(FACTOR_COL_NAME)) |>
  rename(factor_excess_orig = !!FACTOR_COL_NAME) |>
  # JKP data is already excess return, no need to subtract RF
  filter(date >= start_date_ext & date <= end_date_ext)

# 3. Combine monthly returns and lagged variance
portfolios_unscaled <- inner_join(
  mkt_monthly_ext,
  factor_monthly_ext,
  by = "date"
) |>
  inner_join(
    variance_monthly_lag,
    by = "date"
  ) |>
  mutate(
    # Create unscaled returns 
    mkt_excess_unscaled = mkt_excess_orig / mkt_var_lag,
    factor_excess_unscaled = factor_excess_orig / factor_var_lag
  ) |>
  filter(
    !is.na(mkt_excess_unscaled) & !is.na(factor_excess_unscaled) &
      !is.infinite(mkt_excess_unscaled) & !is.infinite(factor_excess_unscaled)
  )

# 4. Calculate scaling constants 'c' 
c_mkt <- sd(portfolios_unscaled$mkt_excess_orig) / sd(portfolios_unscaled$mkt_excess_unscaled)
c_factor <- sd(portfolios_unscaled$factor_excess_orig) / sd(portfolios_unscaled$factor_excess_unscaled)

# 5. Create final scaled portfolios
portfolios_final <- portfolios_unscaled |>
  mutate(
    mkt_excess_managed = c_mkt * mkt_excess_unscaled,
    factor_excess_managed = c_factor * factor_excess_unscaled
  )

print("--- Part 2: Portfolio Scaling Verification ---")
print(paste("Market Scaling 'c':", round(c_mkt, 4)))
print(paste("Original Mkt SD:", round(sd(portfolios_final$mkt_excess_orig), 6)))
print(paste("Managed Mkt SD: ", round(sd(portfolios_final$mkt_excess_managed), 6)))
print(paste("Factor Scaling 'c':", round(c_factor, 4)))
print(paste("Original Factor SD:", round(sd(portfolios_final$factor_excess_orig), 6)))
print(paste("Managed Factor SD: ", round(sd(portfolios_final$factor_excess_managed), 6)))





# 1. Convert our data to XTS objects for PerformanceAnalytics
portfolios_xts <- portfolios_final |>
  select(
    date,
    mkt_excess_orig, mkt_excess_managed,
    factor_excess_orig, factor_excess_managed
  ) |>
  tq_transmute(select = -date, mutate_fun = to.xts)

# 2. Calculate Sharpe Ratios and Max Drawdowns
sharpe_ratios <- SharpeRatio.annualized(portfolios_xts, scale = 12)
max_drawdowns <- maxDrawdown(portfolios_xts)

# 3. Run regressions for Alphas
# We use coeftest for robust (HAC) standard errors
model_capm_orig <- lm(mkt_excess_orig ~ mkt_excess_orig, data = portfolios_final) # Just for consistency, alpha is 0
model_capm_man <- lm(mkt_excess_managed ~ mkt_excess_orig, data = portfolios_final)
model_capm_factor_orig <- lm(factor_excess_orig ~ mkt_excess_orig, data = portfolios_final)
model_capm_factor_man <- lm(factor_excess_managed ~ mkt_excess_orig, data = portfolios_final)

model_rel_mkt <- lm(mkt_excess_managed ~ mkt_excess_orig, data = portfolios_final)
model_rel_factor <- lm(factor_excess_managed ~ factor_excess_orig, data = portfolios_final)

# Helper function to extract annualized % alpha and t-stat
get_alpha_stats <- function(model) {
  robust_test <- coeftest(model, vcov = vcovHAC(model))
  alpha_monthly_frac <- robust_test[1, "Estimate"]
  se_monthly_frac <- robust_test[1, "Std. Error"]
  
  alpha_ann_pct <- alpha_monthly_frac * 12 * 100
  se_ann_pct <- se_monthly_frac * 12 * 100
  t_stat <- robust_test[1, "t value"]
  
  return(
    sprintf("%.2f%% (t=%.2f)", alpha_ann_pct, t_stat)
  )
}

# 4. Assemble the final results table
results_summary <- tibble(
  Portfolio = c(
    "Market (Original)", "Market (Managed)",
    paste(FACTOR_NAME, "(Original)"), paste(FACTOR_NAME, "(Managed)")
  ),
  `Annualized Sharpe Ratio` = c(
    sharpe_ratios["mkt_excess_orig"], sharpe_ratios["mkt_excess_managed"],
    sharpe_ratios["factor_excess_orig"], sharpe_ratios["factor_excess_managed"]
  ),
  `Max Drawdown` = c(
    max_drawdowns["mkt_excess_orig"], max_drawdowns["mkt_excess_managed"],
    max_drawdowns["factor_excess_orig"], max_drawdowns["factor_excess_managed"]
  ),
  `CAPM Alpha (vs Mkt)` = c(
    "0.00% (t=0.00)", # By definition
    get_alpha_stats(model_capm_man),
    get_alpha_stats(model_capm_factor_orig),
    get_alpha_stats(model_capm_factor_man)
  ),
  `Alpha (vs Original)` = c(
    "---",
    get_alpha_stats(model_rel_mkt),
    "---",
    get_alpha_stats(model_rel_factor)
  )
)

# 5. Print the table using 'gt' for professional presentation
final_table <- results_summary |>
  mutate(
    `Annualized Sharpe Ratio` = round(as.numeric(`Annualized Sharpe Ratio`), 3),
    `Max Drawdown` = scales::percent(`Max Drawdown`, accuracy = 0.01)
  ) |>
  gt() |>
  tab_header(
    title = "Part 2: Volatility-Managed Portfolio Performance",
    subtitle = paste("Market vs.", FACTOR_NAME, "| D =", D, "days")
  ) |>
  cols_align(align = "center", columns = -Portfolio) |>
  cols_label(
    `Annualized Sharpe Ratio` = "Annual. Sharpe",
    `Max Drawdown` = "Max Drawdown",
    `CAPM Alpha (vs Mkt)` = "CAPM Alpha",
    `Alpha (vs Original)` = "Alpha vs. Original"
  ) |>
  tab_source_note(
    source_note = "Alphas are annualized (monthly alpha * 12) and in percent. t-statistics are from HAC robust standard errors."
  ) |>
  opt_table_outline()

print(final_table)
