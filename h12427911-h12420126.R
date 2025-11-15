# --- 1. Load All Necessary Packages ---

# List all the packages we'll need for data wrangling, plotting, and analysis
all_packages <- c(
  "dplyr", "tidyr", "lubridate", "stringr",
  "rstudioapi",
  "frenchdata",
  "ggplot2", "scales", "corrplot", "RColorBrewer",
  "tidyquant", "readr",
  "broom",
  "patchwork",
  "lmtest",
  "sandwich",
  "gt"
)

# Set the CRAN mirror
options(repos = "https://cloud.r-project.org")

# Check if each package is installed, and if not, install it.
installed <- rownames(installed.packages())
for(pkg in all_packages) {
  if(! pkg %in% installed) install.packages(pkg)
}

# Load all packages
invisible(lapply(all_packages, library, character.only = TRUE))

# --- 2. Set Working Directory ---
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

############################ PART 1.  ##########################################

# --- 3. Load and Prepare Fama-French Data ---
start_date <- ymd("1926-01-01") 
end_date <- ymd("2015-12-31")   

# Download the daily Fama-French 3-factor data
factors_ff3_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")

# Clean up the downloaded daily data:
# 1. Convert the date column to date format.
# 2. Scale returns from percentages to decimals.
# 3. Rename columns.
# 4. Filter for our Part 1 date range.
factors_ff3_daily <- factors_ff3_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |>
  filter(date >= start_date & date <= end_date)


### STEPS 1 & 2a: Calculate Monthly Returns and Variance ###
# 1. Calculate monthly variance from the daily data.
# 2. Compound the daily returns to get the monthly return.
data_monthly <- factors_ff3_daily |>
  mutate(
    year_month = floor_date(date, "month"), # Create a helper column for grouping
    mkt_return_daily = mkt_excess + rf      # We need total return (not excess) for compounding
  ) |>
  group_by(year_month) |>
  summarize(
    date = max(date), # Use end-of-month date as the identifier for this summary
    
    # --- From Step 1 ---
    mkt_excess_var = var(mkt_excess, na.rm = TRUE),
    smb_var = var(smb, na.rm = TRUE),
    hml_var = var(hml, na.rm = TRUE),
    
    n_days = n(), # Count the number of trading days
    
    # --- From Step 2a ---
    # Compound the daily total returns to get the monthly total return
    mkt_return_comp = prod(1 + mkt_return_daily) - 1,
    # Compound the daily risk-free rates
    rf_comp = prod(1 + rf) - 1,
    mkt_excess_orig = mkt_return_comp - rf_comp,
    
    .groups = "drop"
  ) |>
  select(-year_month)




### STEP 2 (cont.): Construct the Volatility-Managed Portfolio ###

# Lag the variance.
data_unscaled <- data_monthly |>
  mutate(
    mkt_excess_var_lag = lag(mkt_excess_var)
  ) |>
  filter(!is.na(mkt_excess_var_lag)) |> # Drop the first month
  mutate(
    mkt_excess_unscaled = mkt_excess_orig / mkt_excess_var_lag
  )

# Calculate the scaling constant 'c'
# c = vol(original portfolio) / vol(unscaled portfolio)
c_scalar <- sd(data_unscaled$mkt_excess_orig) / sd(data_unscaled$mkt_excess_unscaled)

# Apply the scaling to get the final managed portfolio returns
factors_vol_managed <- data_unscaled |>
  mutate(
    mkt_excess_managed = c_scalar * mkt_excess_unscaled
  )

print(paste("Scaling constant 'c':", round(c_scalar, 6)))
print(paste("Original Portfolio SD:", round(sd(factors_vol_managed$mkt_excess_orig), 6)))
print(paste("Managed Portfolio SD: ", round(sd(factors_vol_managed$mkt_excess_managed), 6)))


### STEP 3: Sort Months into Variance Quintiles ###
# Group all months into 5 buckets (quintiles) based on their lagged variance
factors_vol_managed <- factors_vol_managed |>
  mutate(
    var_quintile = ntile(mkt_excess_var_lag, 5),
    # Create nice labels for the plots
    var_quintile_labeled = factor(
      var_quintile,
      levels = 1:5,
      labels = c("Low Vol", "2", "3", "4", "High Vol"),
      ordered = TRUE
    )
  )

print("Count of months in each variance quintile:")
print(table(factors_vol_managed$var_quintile_labeled))


### STEP 4: Reproduce Figures and Tables ###

# --- 4.A: Load NBER Recession Data ---
# Load theme returns from RData + NBER data
load("h12427911-h12420126.RData")

# Clean up the NBER data
nber_data <- nber_data_raw %>%
  mutate(
    date = ymd(observation_date), 
    date_join = floor_date(date, "month"), # Create a key for joining
    us_recession = as.numeric(USREC) 
  ) %>%
  select(date_join, us_recession)

# Add the recession indicator to our main data frame
factors_vol_managed <- factors_vol_managed %>%
  mutate(
    date_join = floor_date(date, "month") # Create the matching key
  ) %>%
  left_join(nber_data, by = "date_join") %>%
  select(-date_join) 


# --- 4.B: Summary Statistics for Plots ---
# Calculate all the stats needed for the Figure 1 plots
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


# --- 4.C: Create Bar Charts (Replication of Figure 1) ---
# This block plots the original portfolio stats to replicate Figure 1.

# Plot 1: Average Return (Original Portfolio)
p1_orig <- ggplot(quintile_summary, aes(x = var_quintile_labeled, y = mean_ret_orig_ann * 100)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  # Set Y-axis scale to match the paper (0 to 12)
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2)) +
  labs(
    title = "Average Return",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank())

# Plot 2: Standard Deviation (Original Portfolio)
p2_orig <- ggplot(quintile_summary, aes(x = var_quintile_labeled, y = sd_ret_orig_ann * 100)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  # Set Y-axis scale to match the paper (0 to 40)
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  labs(
    title = "Standard Deviation",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank())

# Plot 3: Ratio E[R]/Var(R) (Original Portfolio)
p3_orig <- ggplot(quintile_summary, aes(x = var_quintile_labeled, y = mean_var_orig)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  # Set Y-axis scale to match the paper (0 to 8)
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
  labs(
    title = "E[R]/Var(R)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank())

# Plot 4: Probability of Recession
p4_orig <- ggplot(quintile_summary, aes(x = var_quintile_labeled, y = prob_recession)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  # Set Y-axis scale to match the paper (0 to 0.5)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
  labs(
    title = "Probability of Recession",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.x = element_blank())

# --- Combine all 4 plots using patchwork ---
figure_1_replication <- (p1_orig + p2_orig) / (p3_orig + p4_orig)


# Print the replicated Figure 1
print(figure_1_replication)


# --- 4.C: Create Bar Charts (Comparison) ---
# This is the comparison plot block (Original vs. Managed)

# Define the custom colors
my_colors <- c("Managed" = "#80bef1", "Original" = "darkblue")

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
  scale_fill_manual(values = my_colors) +
  labs(
    title = "Average Return",
    x = "Variance Quintile",
    y = "Annualized Return",
    fill = "Portfolio"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

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
  scale_fill_manual(values = my_colors) + 
  labs(
    title = "Standard Deviation",
    x = "Variance Quintile",
    y = "Annualized Std. Dev.",
    fill = "Portfolio"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

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
  scale_fill_manual(values = my_colors) +
  labs(
    title = "E[R] / Var(R)",
    x = "Variance Quintile",
    y = "Ratio (Monthly)",
    fill = "Portfolio"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

# Plot 4: Probability of being in a Recession
p4 <- ggplot(quintile_summary, aes(x = var_quintile_labeled, y = prob_recession)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Probability of Recession",
    x = "Variance Quintile",
    y = "Probability"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

# --- Combine all 4 plots using patchwork ---
combined_plot <- (p1 + p2) / (p3 + p4) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

# Print the combined plot
print(combined_plot)






# --- 4.D: Table I (Panel A, Column 1) ---
# 1. Run the model: managed_return = alpha + beta * original_return
model_table1 <- lm(mkt_excess_managed ~ mkt_excess_orig, 
                   data = factors_vol_managed)

# 2. Get robust (HAC) coefficients and model statistics
model_coeffs_robust <- coeftest(model_table1, vcov = vcovHAC(model_table1))
model_stats <- broom::glance(model_table1)

# 3. Define the scaling factor (12 months * 100 for percent)
scaling_factor <- 1200 

# 4. Create the final results table
results_table <- tibble(
  Term = c("Alpha (α)", "MktRF (Slope)"),
  Estimate = c(
    model_coeffs_robust[1, "Estimate"] * scaling_factor, 
    model_coeffs_robust[2, "Estimate"]                    
  ),
  StdError = c(
    model_coeffs_robust[1, "Std. Error"] * scaling_factor, 
    model_coeffs_robust[2, "Std. Error"]                    
  )
)

# 5. Report all results in one table
alpha_est <- model_coeffs_robust[1, "Estimate"] * scaling_factor
alpha_se <- model_coeffs_robust[1, "Std. Error"] * scaling_factor
slope_est <- model_coeffs_robust[2, "Estimate"]
slope_se <- model_coeffs_robust[2, "Std. Error"]
rmse <- model_stats$sigma * scaling_factor # RMSE also needs scaling

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
  
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      style = "solid",
      weight = px(2)
    ),
    locations = cells_source_notes()
  ) %>%
  
  opt_table_lines("none") %>% # Clean look
  opt_table_outline(style = "solid", width = px(2)) %>%
  
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black"
  )

# Print the final table
print(comparison_table)

# --- 4.E: --- SCATTER PLOT FOR PART 1 ---
cat("\nGenerating scatter plot for Part 1...\n")

scatter_p1 <- ggplot(factors_vol_managed, aes(x = mkt_excess_orig, y = mkt_excess_managed)) +
  geom_point(alpha = 0.2, color = "blue") +
  # Add the 45-degree (y=x) line for reference
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Part 1: Managed Market vs. Original Market Returns (1926-2015)",
    x = "Original Market Excess Return",
    y = "Managed Market Return"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

print(scatter_p1)


############################ PART 2.  ##########################################

# Define start and end dates for analysis
start_date_part_2 <- ymd("1926-01-01") # Match factor data availability 
end_date_part_2 <- ymd("2025-07-31")   # Match factor data availability 

# --- 1. Load and Prepare Fama-French Data ---

factors_ff3_daily_raw_part_2 <- download_french_data("Fama/French 3 Factors [Daily]")

factors_ff3_daily_part_2 <- factors_ff3_daily_raw_part_2$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |>
  filter(date >= start_date_part_2 & date <= end_date_part_2)

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

    mkt_excess_var = var(mkt_excess, na.rm = TRUE),
    smb_var = var(smb, na.rm = TRUE),
    hml_var = var(hml, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  arrange(date) |>
  select(date, mkt_excess_var, smb_var, hml_var)

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

# --- Step 1: Setup ---
# 1. Define parameters based on student ID
digit_7 <- 2
digit_8 <- 6
D <- digit_7 + digit_8 + 10
print(paste("Using D =", D))

FACTOR_NAME <- "momentum" 
FACTOR_COL_NAME <- paste0("ret_usa_", FACTOR_NAME)

# 2. Update end date and load full Fama-French daily data
start_date_ext <- ymd("1926-07-01")
end_date_ext <- ymd("2025-07-31") # As per assignment 

# Get the full-sample daily market data
mkt_daily_ext <- factors_ff3_daily_raw_part_2$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    mkt_excess = as.numeric(`Mkt-RF`) / 100,
    rf = as.numeric(RF) / 100 
  ) |>
  select(date, mkt_excess, rf) |>
  filter(date >= start_date_ext & date <= end_date_ext)

# Get the daily factor data
if (!FACTOR_COL_NAME %in% names(all_themes_daily_vw_cap_wide_usa)) {
  stop(paste("Factor column", FACTOR_COL_NAME, "not found."))
}

factor_daily_ext <- all_themes_daily_vw_cap_wide_usa |>
  select(date, all_of(FACTOR_COL_NAME)) |>
  rename(factor_excess = !!FACTOR_COL_NAME) |>
  filter(date >= start_date_ext & date <= end_date_ext)

# 3. Join Mkt and Factor daily data
daily_data <- inner_join(mkt_daily_ext, factor_daily_ext, by = "date")

# --- Part 2: Step 1 ---

# Define the custom variance function to match the assignment's formula:
# formula: sigma_hat^2 = (D/22) * Sum_of_Squared_Deviations

calculate_assignment_var <- function(daily_returns_window) {
  
  # D_global is the target window size defined earlier.
  D_global <- D 
  
  # Get the number of non-missing observations in the current window
  n_valid_obs <- sum(!is.na(daily_returns_window))

  if (n_valid_obs < 2) {
    return(NA_real_)
  }
  
  # 1. Calculate the sample variance
  # s^2 = Sum_of_Squared_Deviations / (n - 1)
  s2 <- var(daily_returns_window, na.rm = TRUE)
  
  # 2. Back out the Sum of Squared Deviations (SSD)
  # SSD = s^2 * (n - 1)
  sum_sq_dev <- s2 * (n_valid_obs - 1)
  
  # 3. Apply the assignment's full formula: (D/22) * SSD
  # The scaling factor (D/22) uses the target window size, D_global
  assignment_var <- (D_global / 22) * sum_sq_dev
  
  return(assignment_var)
}


# 4. Calculate D-day rolling variance using the assignment's formula
daily_data_with_var <- daily_data |>
  arrange(date) |>
  tq_mutate(
    select = mkt_excess,
    mutate_fun = rollapply,
    width = D,
    FUN = calculate_assignment_var, 
    align = "right",
    fill = NA,
    col_rename = "mkt_d_day_var" 
  ) |>
  tq_mutate(
    select = factor_excess,
    mutate_fun = rollapply,
    width = D,
    FUN = calculate_assignment_var,
    align = "right",
    fill = NA,
    col_rename = "factor_d_day_var"
  ) |>
  filter(!is.na(mkt_d_day_var) & !is.na(factor_d_day_var))

# 5. Get end-of-month variance to match with next month's return
variance_monthly_lag <- daily_data_with_var |>
  mutate(year_month = floor_date(date, "month")) |>
  group_by(year_month) |>
  summarize(
    # Get the last D-day variance from that month
    mkt_var_lag = last(mkt_d_day_var),
    factor_var_lag = last(factor_d_day_var),
    .groups = "drop"
  ) |>
  mutate(
    # This variance from month 't' will be used for month 't+1' returns
    date = year_month + months(1)
  ) |>
  select(date, mkt_var_lag, factor_var_lag)


# --- Part 2: Step 2 ---
# 1. Load monthly Mkt returns (compounded from daily data)
mkt_monthly_ext <- daily_data |>
  mutate(
    mkt_return = mkt_excess + rf
  ) |>
  group_by(year_month = floor_date(date, "month")) |>
  summarize(
    mkt_return_comp = prod(1 + mkt_return) - 1,
    rf_comp = prod(1 + rf) - 1,
    mkt_excess_orig = mkt_return_comp - rf_comp,
    .groups = "drop"
  ) |>
  rename(date = year_month) |> 
  select(date, mkt_excess_orig)

# 2. Load monthly Factor returns
factor_monthly_ext <- all_themes_monthly_vw_cap_wide_usa |>
  select(date, all_of(FACTOR_COL_NAME)) |>
  rename(factor_excess_orig = !!FACTOR_COL_NAME) |>
  filter(date >= start_date_ext & date <= end_date_ext) |>
  mutate(date = floor_date(date, "month")) |> 
  select(date, factor_excess_orig)

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




# --- Part 2: Step 3 (Calculate Metrics) ---

# 1. Convert our data to XTS objects
portfolios_matrix <- portfolios_final |>
  select(
    mkt_excess_orig, mkt_excess_managed,
    factor_excess_orig, factor_excess_managed
  ) |>
  as.matrix()

portfolios_dates <- portfolios_final$date
portfolios_xts <- xts::xts(portfolios_matrix, order.by = portfolios_dates)

print("Successfully created XTS object:")
print(head(portfolios_xts))

# 2. Calculate Sharpe Ratios and Max Drawdowns
sharpe_ratios <- PerformanceAnalytics::SharpeRatio.annualized(portfolios_xts, scale = 12)
max_drawdowns <- PerformanceAnalytics::maxDrawdown(portfolios_xts)

# 3. Run regressions for Alphas
model_capm_man <- lm(mkt_excess_managed ~ mkt_excess_orig, data = portfolios_final)
model_capm_factor_orig <- lm(factor_excess_orig ~ mkt_excess_orig, data = portfolios_final)
model_capm_factor_man <- lm(factor_excess_managed ~ mkt_excess_orig, data = portfolios_final)

# Alphas relative to their own original (unmanaged) factor
model_rel_mkt <- lm(mkt_excess_managed ~ mkt_excess_orig, data = portfolios_final)
model_rel_factor <- lm(factor_excess_managed ~ factor_excess_orig, data = portfolios_final)

# Helper function
get_alpha_stats <- function(model) {
  # Use Newey-West (HAC) standard errors for robustness
  robust_test <- coeftest(model, vcov = vcovHAC(model))
  
  alpha_monthly_frac <- robust_test[1, "Estimate"]
  se_monthly_frac <- robust_test[1, "Std. Error"]
  t_stat <- robust_test[1, "t value"]
  
  alpha_ann_pct <- alpha_monthly_frac * 12 * 100
  
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
    sharpe_ratios[,"mkt_excess_orig"], sharpe_ratios[,"mkt_excess_managed"],
    sharpe_ratios[,"factor_excess_orig"], sharpe_ratios[,"factor_excess_managed"]
  ),
  `Max Drawdown` = c(
    max_drawdowns[,"mkt_excess_orig"], max_drawdowns[,"mkt_excess_managed"],
    max_drawdowns[,"factor_excess_orig"], max_drawdowns[,"factor_excess_managed"]
  ),
  `CAPM Alpha (vs Mkt)` = c(
    "0.00% (t=0.00)",
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

# 5. Print the table using 'gt'
final_table <- results_summary |>
  mutate(
    `Annualized Sharpe Ratio` = round(as.numeric(`Annualized Sharpe Ratio`), 3),
    `Max Drawdown` = scales::percent(`Max Drawdown`, accuracy = 0.01)
  ) |>
  gt() |>
  tab_header(
    title = "Part 2: Volatility-Managed Portfolio Performance",
    subtitle = paste("Market vs.", str_to_title(FACTOR_NAME), "| D =", D, "days")
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


# --- Part 2: Step 4 (Scatter Plots) ---
cat("\nGenerating scatter plots for Part 2...\n")

scatter_p2_mkt <- ggplot(portfolios_final, aes(x = mkt_excess_orig, y = mkt_excess_managed)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Part 2: Managed Market vs. Original Market Returns",
    x = "Original Market Excess Return",
    y = "Managed Market Return"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

scatter_p2_factor <- ggplot(portfolios_final, aes(x = factor_excess_orig, y = factor_excess_managed)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = paste("Part 2: Managed", str_to_title(FACTOR_NAME), "vs. Original Factor"),
    x = "Original Factor Excess Return",
    y = "Managed Factor Return"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# Combine and print scatter plots
scatter_plots_p2 <- scatter_p2_mkt + scatter_p2_factor
print(scatter_plots_p2)

# --- Part 2: Step 4 (Density Plots) ---

# --- 1: (1926-2015) ---

# Pivot the Part 1 data to a long format for ggplot
density_data_p1 <- factors_vol_managed %>%
  select(mkt_excess_orig, mkt_excess_managed) %>%
  pivot_longer(
    cols = everything(),
    names_to = "portfolio_type",
    values_to = "returns"
  ) %>%
  mutate(
    portfolio_type = if_else(
      portfolio_type == "mkt_excess_orig", 
      "Original", 
      "Managed"
    )
  )

# Plot the Part 1 densities
density_plot_p1 <- ggplot(density_data_p1, 
                          aes(x = returns, fill = portfolio_type)) +
  geom_density(alpha = 0.5) +
  
  scale_fill_manual(values = my_colors) +
  
  scale_x_continuous(
    labels = scales::percent_format(),
    limits = c(-0.3, 0.3)
  ) +
  labs(
    title = "Part 1: Return Distribution (1926-2015)",
    subtitle = "Managed portfolio has thinner tails and a higher peak.",
    x = "Monthly Excess Return",
    y = "Density",
    fill = "Portfolio Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(density_plot_p1)


# --- 2: Extended Sample ---

my_4_colors <- c(
  "Market (Original)" = "black",
  "Market (Managed)" = "#80bef1",
  "Momentum Factor (Original)" = "black",
  "Momentum Factor (Managed)" = "green4"
)

# Pivot the Part 2 data to a long format
density_data_p2 <- portfolios_final %>%
  select(mkt_excess_orig, mkt_excess_managed, 
         factor_excess_orig, factor_excess_managed) %>%
  pivot_longer(
    cols = everything(),
    names_to = "portfolio_key",
    values_to = "returns"
  ) %>%
  mutate(
    Portfolio = case_when(
      grepl("mkt_", portfolio_key) ~ "Market",
      grepl("factor_", portfolio_key) ~ paste(str_to_title(FACTOR_NAME), "Factor")
    ),
    Type = case_when(
      grepl("_orig", portfolio_key) ~ "Original",
      grepl("_managed", portfolio_key) ~ "Managed"
    ),

    portfolio_fill = paste(Portfolio, Type, sep=" (") %>% paste0(")"),
    
    portfolio_fill = factor(portfolio_fill, levels = c(
      "Market (Original)",
      "Market (Managed)",
      "Momentum Factor (Original)",
      "Momentum Factor (Managed)"
    ))
  )

density_plot_p2 <- ggplot(density_data_p2,                        
                          aes(x = returns, fill = portfolio_fill)) +
  geom_density(alpha = 0.5) + 
  facet_wrap(~ Portfolio, scales = "free") + 
  scale_fill_manual(values = my_4_colors) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Return Distributions (Extended Sample)",
    subtitle = "Volatility management reduces tail risk in both portfolios.",
    x = "Monthly Excess Return",
    y = "Density",
    fill = "Portfolio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(density_plot_p2)

# --- 2: Extended Sample (Separated Plots) ---

my_4_colors <- c(
  "Market (Original)" = "black",
  "Market (Managed)" = "#80bef1",
  "Momentum Factor (Original)" = "black",
  "Momentum Factor (Managed)" = "green4"
)

density_data_p2 <- portfolios_final %>%
  select(mkt_excess_orig, mkt_excess_managed, 
         factor_excess_orig, factor_excess_managed) %>%
  pivot_longer(
    cols = everything(),
    names_to = "portfolio_key",
    values_to = "returns"
  ) %>%
  mutate(
    Portfolio = case_when(
      grepl("mkt_", portfolio_key) ~ "Market",
      grepl("factor_", portfolio_key) ~ paste(str_to_title(FACTOR_NAME), "Factor")
    ),
    Type = case_when(
      grepl("_orig", portfolio_key) ~ "Original",
      grepl("_managed", portfolio_key) ~ "Managed"
    ),
    
    portfolio_fill = paste(Portfolio, Type, sep=" (") %>% paste0(")"),
    
    portfolio_fill = factor(portfolio_fill, levels = c(
      "Market (Original)",
      "Market (Managed)",
      "Momentum Factor (Original)",
      "Momentum Factor (Managed)"
    ))
  )

density_plot_p2_mkt <- density_data_p2 %>%
  filter(Portfolio == "Market") %>%
  ggplot(aes(x = returns, fill = portfolio_fill)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = my_4_colors) + 
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Return Distribution: Market (Extended Sample)",
    subtitle = "Volatility management reduces tail risk.",
    x = "Monthly Excess Return",
    y = "Density",
    fill = "Portfolio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

density_plot_p2_factor <- density_data_p2 %>%
  filter(Portfolio == "Momentum Factor") %>%
  ggplot(aes(x = returns, fill = portfolio_fill)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = my_4_colors) + 
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Return Distribution: Momentum Factor (Extended Sample)",
    subtitle = "Volatility management dramatically tames crash risk.",
    x = "Monthly Excess Return",
    y = "Density",
    fill = "Portfolio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(density_plot_p2_mkt)
print(density_plot_p2_factor)

# --- Part 2: Step 4 (Cumulative Return Plot) ---

# Prepare data for cumulative plotting
plot_data_cumulative <- portfolios_final %>%
  select(date, 
         `Market (Original)` = mkt_excess_orig, 
         `Market (Managed)` = mkt_excess_managed,
         `Momentum (Original)` = factor_excess_orig,
         `Momentum (Managed)` = factor_excess_managed
  ) %>%
  
  # Pivot to long format
  pivot_longer(
    cols = -date,
    names_to = "portfolio",
    values_to = "returns"
  ) %>%
  
  # Calculate cumulative (geometric) returns
  group_by(portfolio) %>%
  mutate(
    cumulative_return = cumprod(1 + returns) 
  ) %>%
  ungroup() %>%
  
  mutate(
    portfolio = factor(portfolio, levels = c(
      "Market (Original)", "Market (Managed)", 
      "Momentum (Original)", "Momentum (Managed)"
    ))
  )

cumulative_plot <- ggplot(plot_data_cumulative, 
                          aes(x = date, 
                              y = cumulative_return, 
                              color = portfolio)) +
  geom_line(linewidth=1) +
  # log scale for the y-axis
  scale_y_log10(
    labels = scales::dollar_format(prefix = "", suffix = "x") 
  ) +
  scale_color_manual(values = c(
    "Market (Original)" = "darkblue",
    "Market (Managed)" = "#80bef1",
    "Momentum (Original)" = "darkgreen",
    "Momentum (Managed)" = "green4"
  )) +
  
  labs(
    title = "Cumulative Excess Returns (1926 - 2025)",
    subtitle = "Volatility-Managed vs. Original Portfolios (Log Scale)",
    x = "Date",
    y = "Cumulative Return (Log Scale)",
    color = "Portfolio"
  ) +  
  theme_minimal() +
  theme(legend.position = "bottom")
print(cumulative_plot)

