## 1. Loading packages
## ----------------------
library(dplyr) #For datastructuring / cleaning
library(ggplot2) #Plots and visualisatin for explorative analysis and performance 
library(extRemes) #Main package, use for models in extreme value theory
library(scales) #Formatting for plots, used in combination with ggplot2
library(patchwork) #To combine several plots into one figure
library(gtable) #Mainly for table construction
library(readr) #For loading the dataset into R
library(evd)

packageVersion("gtable")







## 2. Clean datat
## ----------------
breaches_raw <- read.csv("data_breaches.csv") #Loading of data

breaches <- breaches_raw %>%
  select(year, records.lost) %>%
  mutate(
    year = as.integer(year),
    #Erase everything expect for actual numbers to make the data numeric
    records_lost_num = as.numeric(gsub("[^0-9]", "", records.lost))
  ) %>%
  filter(
    !is.na(year),
    !is.na(records_lost_num),
    records_lost_num > 0
  ) %>%
  mutate(
    log10_records = log10(records_lost_num)  #Apply log10 scale of the breaches
  )

summary(breaches$records_lost_num) #Overlook of the data

View(breaches) #Quick inspection








## 3. Create block annual maxima in dataset
## ---------------------------------------

#Block size are one year, and in each block the largest observation (annual maximum) is extracted
#Isolate the largest data breaches for each year
max_yearly <- breaches %>%
  group_by(year) %>%
  summarise(max_records = max(records_lost_num, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(year) %>%
  mutate(
    max_records_millions = max_records / 1e6,
    log10_max_records    = log10(max_records)
  )

write_csv(max_yearly, "max_yearly.csv") #New dataset for the EVT analysis

View(max_yearly) #Quick inspection of the annual maxima observations 








## 4. Explorative analysis - Plots
## ---------------------------------

# 4.1 Timeseries of yearly maxima with log10 on the y-axis
p_time <- ggplot(max_yearly, aes(x = year, y = max_records)) +
  geom_line(linewidth = 0.9, color = "darkblue") +
  geom_point(size = 2.5, color = "darkblue") +
  scale_y_log10(
    labels = label_number(scale_cut = cut_si("M")) #Show amounts in millions of breaches
  ) +
  scale_x_continuous(
    breaks = seq(min(max_yearly$year), max(max_yearly$year), by = 2)
  ) +
  labs(
    title = "Biggest databreaches per year",
    subtitle = "Y_axis in log10-scale, amount in millions of units",
    x = "Year",
    y = "Max records lost"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

#Visualization of the yearly maxima plot
p_time 

## 4.2 Histogram with a density curve containing all breaches in the dataset
p_hist_all <- ggplot(breaches, aes(x = log10_records)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    alpha = 0.6,
    fill  = "steelblue",
    color = "white"
  ) +
  geom_density(linewidth = 1) +
  labs(
    title = "Distribution over all databreaches",
    subtitle = "Log10-transformed in amount of exposed records",
    x = "log10(records_lost_num)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )
#Visualization of the histogram plot including a density function
p_hist_all #Rightly scewed distribution, more dense in more recent years

## 4.3 Scatterplot over time for all databreaches in log10-scale
p_scatter_all <- ggplot(breaches, aes(x = year, y = log10_records)) +
  geom_point(alpha = 0.4, size = 2.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.5, color = "darkblue") +
  labs(
    title = "Databreaches over time",
    subtitle = "Total breaches log10-transformed",
    x = "Year",
    y = "log10(records_lost_num)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold")
  )
#Visualization of the scatterplot over time including all databreaches
p_scatter_all #The regression line does not seem to show any specfific time trend in
# overall size of the breaches over time

## 4.4 Trend between yearly maximum breach in log10-scale
p_scatter_max <- ggplot(max_yearly, aes(x = year, y = log10_max_records)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1, color = "darkblue") +
  labs(
    title = "Trend in yearly maximum breaches",
    subtitle = "Log10-transformed blockmaxima per year",
    x = "Year",
    y = "log10(max_records)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )
#Visualization of the trend plot between yearly maximums
p_scatter_max #From the plot, it seems to be a clear time trend in yearly maximums
# where observation values has increased in recent years. 

## 4.5 Amounts of breaches per year
yearly_counts <- breaches %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n_breaches = dplyr::n(),
    .groups = "drop"
  )

p_counts <- ggplot(yearly_counts, aes(x = year, y = n_breaches)) +
  geom_col(alpha = 0.7, fill = "steelblue") +
  scale_x_continuous(
    breaks = seq(min(yearly_counts$year), max(yearly_counts$year), by = 2)
  ) +
  labs(
    title = "Amount of databreaches per year",
    x = "Year",
    y = "Amount of breaches"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )
#Visualization of plot over amounts of databreaches per year
p_counts #Amount of databreaches seem to in increase as time goes

## 4.6 Log-log tailplot for the tail in the data
#Uses raw amounts of records_lost_num, not in log10-scale, just strictly positive
tail_df <- breaches %>%
  dplyr::filter(records_lost_num > 0) %>%
  dplyr::arrange(records_lost_num) %>%
  dplyr::mutate(
    rank = dplyr::row_number(),
    n    = dplyr::n(),
    # Empirical survivalfunction, P(X > x)
    survival = 1 - (rank - 0.5) / n,
    log10_size     = log10(records_lost_num),
    log10_survival = log10(survival)
  ) %>%
  dplyr::filter(is.finite(log10_survival), is.finite(log10_size))

p_tail <- ggplot(tail_df, aes(x = log10_size, y = log10_survival)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "darkblue") +
  labs(
    title = "Log-log tailplot for databreaches",
    subtitle = "Empirical survivalfunction in log10-log10-scale",
    x = "log10(size of breach)",
    y = "log10(P(X > x))"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )
#Visualization of the log-log tailplot 
p_tail #Most observation seem to follow a linear trend, indicating a heavy - tailed 
# behavior in the upper tail.

## 4.7 Descriptive statistics
#Combining describtive statistics in one table
desc_overall <- breaches %>%
  dplyr::summarise(
    n_intrang   = dplyr::n(),
    mean_records   = mean(records_lost_num, na.rm = TRUE),
    median_records = median(records_lost_num, na.rm = TRUE),
    sd_records     = sd(records_lost_num, na.rm = TRUE),
    q1_records     = quantile(records_lost_num, 0.25, na.rm = TRUE),
    q3_records     = quantile(records_lost_num, 0.75, na.rm = TRUE),
    max_records    = max(records_lost_num, na.rm = TRUE)
  )

desc_overall #Overview over descriptive statistics table

#Combined plot visualizations
(p_time / p_scatter_max) +
  plot_annotation()

(p_hist_all / p_scatter_all)

(p_counts / p_tail)








## 6. Stationary GEV - model
##-------------------------------

# extRemes::fevd for GEV on the blockmaxima

max_millions <- max_yearly$max_records / 1e6

fit_stat <- fevd(max_millions, type = "GEV", method = "MLE")

summary(fit_stat) #Summary over stationary model, with estimated parameters, AIC and BIC - values, etc.

plot(fit_stat) #Combined diagnostic plots, QQ-plot, etc. 









## 7. Non-stationary GEV - model (trend in location)
## ----------------------------------------------

fit_ns <- fevd(max_millions,
               data = max_yearly,
               type = "GEV",
               location = ~ year,   # µ_t = a + b * year
               method = "MLE")

summary(fit_ns) #Summary over non - stationary model, with estimated parameters, AIC - values, etc.
plot(fit_ns) #Combined diagnostic plots









## 8. Model - comparison
## -------------------------

summary(fit_stat)$AIC
summary(fit_ns)$AIC

#Likelihood ratio - test (LRT) function inside extRemes package
lr.test(fit_stat, fit_ns, alpha = 0.05)









## 9. Return level estimation (50- and 100-year events)
## ----------------------------------------------------
# 50- and 100-year estimations with stationary modell
rl_stat <- return.level(fit_stat, return.period = c(50, 100))
rl_stat

# 50- and 100-year estimations with non - stationary model, where 2035 is the conditioned time point
newdat_2035 <- data.frame(year = 2035)
rl_ns_2035 <- return.level(fit_ns, return.period=c(50,100), newdata=newdat_2035)
rl_ns_2035









## 10. Bootstrap of the 100-year levels (stationary model)
## -------------------------------------------------------

set.seed(02116)
B <- 1000

x0 <- max_millions
n  <- length(x0)

boot_rl100 <- rep(NA_real_, B)
boot_xi    <- rep(NA_real_, B)

for (b in 1:B) {
  idx <- sample(seq_len(n), size = n, replace = TRUE)
  x_b <- x0[idx]
  
  fit_b <- try(fevd(x_b, type = "GEV", method = "MLE"), silent = TRUE)
  if (!inherits(fit_b, "try-error")) {
    
    boot_xi[b] <- unname(fit_b$results$par["shape"])
    
    rl <- try(return.level(fit_b, return.period = 100), silent = TRUE)
    if (!inherits(rl, "try-error") && is.finite(rl)) {
      boot_rl100[b] <- as.numeric(rl)
    }
  }
}

boot_rl100 <- na.omit(boot_rl100)
boot_xi    <- na.omit(boot_xi)

boot_ok <- boot_xi > -0.5 & boot_xi < 2
mean(!boot_ok)  #Proportion of "breakdowns"

#Robust summary
quantile(boot_rl100, c(0.025, 0.5, 0.975)) #Very broad confidence interval

#Trim, removing the 0.5% largest (because the earlier were so broad), to se how it changes
q <- quantile(boot_rl100, 0.995)
boot_rl100_trim <- boot_rl100[boot_rl100 <= q]
quantile(boot_rl100_trim, c(0.025, 0.5, 0.975)) #The maximum decreases from 50 to 30 billion

#Shape - diagnostic
summary(boot_xi)
mean(boot_xi > 0.95)









## 11. Function to simulate one scenario
## --------------------------------------

simulate_gev_scenario <- function(n_years, n_rep,
                                  mu0, beta, sigma, xi,
                                  return_period = 100) {
  # n_years  = Amount of annual blocks
  # n_rep    = Amount of simulations
  # mu_t     = mu0 + beta * t
  # sigma, xi = Other GEV-parameters
  
  true_years <- 1:n_years
  true_mu <- mu0 + beta * true_years
  
  #Vector to save the 100-year estimation
  rl_hat_stat <- numeric(n_rep)
  rl_hat_ns   <- numeric(n_rep)
  
  for (r in 1:n_rep) {
    # 1. Generating data
    x <- numeric(n_years)
    for (t in 1:n_years) {
      x[t] <- rgev(1, loc = true_mu[t], scale = sigma, shape = xi)
    }
    
    # 2. Fit the stationary model
    fit_s <- try(fevd(x, type = "GEV", method = "MLE"), silent = TRUE)
    
    # 3. Fit the non - stationary model (trend in location)
    fit_n <- try(fevd(x, type = "GEV",
                      location = ~ true_years,
                      method = "MLE"), silent = TRUE)
    
    # 4. Calculate return levels (if fit succeeded) 
    if (!inherits(fit_s, "try-error")) {
      rl_hat_stat[r] <- as.numeric(return.level(fit_s, return.period = return_period))
    } else {
      rl_hat_stat[r] <- NA
    }
    
    if (!inherits(fit_n, "try-error")) {
      # For non - stationary model: ex. for the last year
      newdat_last <- data.frame(true_years = n_years)
      rl_hat_ns[r] <- as.numeric(return.level(fit_n,
                                              return.period = return_period,
                                              newdata = newdat_last))
    } else {
      rl_hat_ns[r] <- NA
    }
  }
  
  data.frame(
    rl_stat = rl_hat_stat,
    rl_ns   = rl_hat_ns
  )
}








## 12. Run an example scenario
## ----------------------------
set.seed(123)

sim_res <- simulate_gev_scenario(
  n_years = 30, 
  n_rep   = 500,
  mu0     = 10,
  beta    = 0.1,   #Positive trend
  sigma   = 1,
  xi      = 0.2,
  return_period = 100
)

summary(sim_res)

# Remove eventuall failed fit (NA:s)
sim_res_clean <- sim_res %>%
  dplyr::filter(is.finite(rl_stat), is.finite(rl_ns))

# Compare with the true 100 - year level (can be analytically calculated or approximate through big simulation)
# Here we only look at the difference between the stationary and non - stationary model
apply(sim_res_clean, 2, median)
apply(sim_res_clean, 2, sd)









## 13. Approximated "true" return level through a big simulation (Monte Carlo)
## --------------------------------------------------------------------------

# Approximated the 100 - year level for the last year (t = n_years)
# Also (for comparison) a stationary case (beta = 0)

approx_true_rl_lastyear <- function(n_mc, mu0, beta, sigma, xi, n_years, return_period = 100) {
  # Simulate many independent maxima for the last year t = n_years
  mu_last <- mu0 + beta * n_years
  x_last  <- rgev(n_mc, loc = mu_last, scale = sigma, shape = xi)
  # Empirical return level: quantile at p = 1 - 1/T
  p <- 1 - 1 / return_period
  as.numeric(quantile(x_last, probs = p, na.rm = TRUE))
}

# Parameters for the scenario above: 
n_years <- 30
mu0     <- 10
beta    <- 0.1
sigma   <- 1
xi      <- 0.2
Tret    <- 100

set.seed(123)
true_rl_last <- approx_true_rl_lastyear(
  n_mc = 200000,          # Use big for a stable approximation, but still quick
  mu0 = mu0, beta = beta, sigma = sigma, xi = xi,
  n_years = n_years,
  return_period = Tret
)

true_rl_last








## 14. Bias & RMSE for stationary vs non - stationary
## --------------------------------------------------
# Cleaning the NA:s
sim_res_clean <- sim_res %>%
  dplyr::filter(is.finite(rl_stat), is.finite(rl_ns))

# Bias = mean(estimate - true)
bias_stat <- mean(sim_res_clean$rl_stat - true_rl_last)
bias_ns   <- mean(sim_res_clean$rl_ns   - true_rl_last)

# RMSE
rmse_stat <- sqrt(mean((sim_res_clean$rl_stat - true_rl_last)^2))
rmse_ns   <- sqrt(mean((sim_res_clean$rl_ns   - true_rl_last)^2))

perf_summary <- data.frame(
  model = c("Stationary GEV", "Non-stationary GEV (location ~ year)"),
  true_RL_lastyear = true_rl_last,
  mean_estimate = c(mean(sim_res_clean$rl_stat), mean(sim_res_clean$rl_ns)),
  bias = c(bias_stat, bias_ns),
  rmse = c(rmse_stat, rmse_ns),
  sd_estimate = c(sd(sim_res_clean$rl_stat), sd(sim_res_clean$rl_ns))
)

perf_summary # Summary over models for comparison








## 15. Visualize the error distribution
##---------------------------------------
err_df <- dplyr::bind_rows(
  data.frame(model = "Stationary", error = sim_res_clean$rl_stat - true_rl_last),
  data.frame(model = "Non-stationary", error = sim_res_clean$rl_ns   - true_rl_last)
)

ggplot(err_df, aes(x = error)) +
  geom_histogram(bins = 35) +
  facet_wrap(~ model, scales = "free_y") +
  labs(title = "Simulation error distribution (estimate - true return level)",
       x = "Error",
       y = "Frequency")
