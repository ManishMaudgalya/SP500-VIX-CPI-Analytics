
# TIME SERIES FINAL PROJECT
# Topic:    Does CPI (inflation) drive S&P 500 returns and VIX (volatility)?
# Data:     S&P 500 (^GSPC), VIX (^VIX) via quantmod | CPI via FRED (fredr)
# Freq:     Monthly (daily series aggregated to month-end)
# Author:   [Your Name]


# --- 0. PACKAGES --------------------------------------------------------------
# Install if needed:
# install.packages(c("quantmod","fredr","tseries","forecast","vars",
#                    "astsa","ggplot2","dplyr","zoo","lubridate","urca"))

library(quantmod)
library(fredr)
library(tseries)
library(forecast)
library(vars)
library(astsa)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(urca)


# STEP 1 — DESCRIBE THE DATA, VARIABLES, AND HYPOTHESES

# VARIABLES
#   CPI   : Consumer Price Index (All Urban Consumers, NSA) — FRED series CPIAUCNS
#           Proxy for inflation. Input / explanatory variable.
#           Transformed: log(CPI) → first difference = monthly inflation rate
#   SP500 : S&P 500 adjusted close (^GSPC, month-end) → log price for plotting
#           → log returns (diff of log) for modelling (stationarity)
#           Adjusted close corrects for dividends and stock splits.
#   VIX   : CBOE Volatility Index adjusted close (^VIX, month-end)
#           → log(VIX) for plotting (reduces right skew)
#           → diff(log(VIX)) for modelling (stationarity)
#           Proxy for market fear / implied volatility. Output variable.
#
# SAMPLE PERIOD: January 2000 – December 2023 (288 monthly observations)
#
# HYPOTHESES
#   H1: Rising CPI (inflation) negatively affects S&P 500 log returns
#       (higher inflation → Fed tightening → lower equity prices)
#   H2: Rising CPI positively affects VIX
#       (inflation uncertainty → higher implied volatility)
#   H3: There are significant lagged effects of CPI on both outputs
#       (policy transmission takes several months)

cat("=== STEP 1: Data description and hypotheses printed above in comments ===\n")



# Plot Data
fredr_set_key("328ac77891678f21c7db833649d1d438")

start_date <- as.Date("2000-01-01")
end_date   <- as.Date("2023-12-31")

# --- 2b. Download S&P 500 and VIX from Yahoo Finance (adjusted close) --------
getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
getSymbols("^VIX",  src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Month-end ADJUSTED close prices
# Ad() extracts the adjusted close column (accounts for dividends & splits)
sp500_monthly_adj <- to.monthly(Ad(GSPC), indexAt = "lastof", OHLC = FALSE)[, 1]
vix_monthly_adj   <- to.monthly(Ad(VIX),  indexAt = "lastof", OHLC = FALSE)[, 1]

# Log prices (for plotting — shows growth on a ratio scale)
log_sp500 <- log(sp500_monthly_adj)
log_vix   <- log(vix_monthly_adj)

# Stationary series for modelling:
#   SP500: log returns = diff(log price)  [~ percentage return]
#   VIX:  diff(log VIX)                  [~ percentage change in volatility]
sp500_ret <- diff(log_sp500)   # first difference of log price
vix_ret   <- diff(log_vix)     # first difference of log VIX
sp500_ret <- sp500_ret[-1]     # drop leading NA from diff()
vix_ret   <- vix_ret[-1]

# --- 2c. Download CPI from FRED -----------------------------------------------
cpi_raw <- fredr(series_id   = "CPIAUCNS",
                 observation_start = start_date,
                 observation_end   = end_date)

cpi_zoo <- zoo(cpi_raw$value, order.by = as.yearmon(cpi_raw$date))

# CPI month-over-month log change (inflation rate)
cpi_inf <- diff(log(cpi_zoo))

# --- 2d. Align all three series to common dates ------------------------------
common_idx <- intersect(intersect(index(sp500_ret), index(vix_ret)),
                        index(cpi_inf))

sp500_ret  <- sp500_ret[common_idx]
vix_ret    <- vix_ret[common_idx]
cpi_inf    <- cpi_inf[common_idx]

# Also align log-price series for plotting (trim to same window)
log_sp500  <- log_sp500[intersect(index(log_sp500), common_idx)]
log_vix    <- log_vix[intersect(index(log_vix), common_idx)]

n <- length(common_idx)
cat(sprintf("\nCommon sample: %s to %s  (%d monthly observations)\n",
            start(sp500_ret), end(sp500_ret), n))

# --- 2e. Convert to ts objects -----------------------------------------------
start_yr  <- as.numeric(format(as.Date(index(sp500_ret)[1]), "%Y"))
start_mo  <- as.numeric(format(as.Date(index(sp500_ret)[1]), "%m"))

# Modelling series (stationary — used in Steps 3–5)
SP500_ts  <- ts(as.numeric(sp500_ret), start = c(start_yr, start_mo), frequency = 12)
VIX_ts    <- ts(as.numeric(vix_ret),   start = c(start_yr, start_mo), frequency = 12)
CPI_ts    <- ts(as.numeric(cpi_inf),   start = c(start_yr, start_mo), frequency = 12)

# Log-price series (non-stationary — used for plotting only)
LOG_SP500_ts <- ts(as.numeric(log_sp500), start = c(start_yr, start_mo), frequency = 12)
LOG_VIX_ts   <- ts(as.numeric(log_vix),   start = c(start_yr, start_mo), frequency = 12)

# --- 2f. PLOTS ---------------------------------------------------------------
# Panel 1: Log-price / log-level series (non-stationary, for visual context)
par(mfrow = c(3, 1), mar = c(3, 4, 3, 1))

plot(LOG_SP500_ts, main = "S&P 500 — Log Adjusted Close (level)",
     ylab = "log(Price)", xlab = "", col = "darkgreen", lwd = 1.2)

plot(LOG_VIX_ts, main = "VIX — Log Adjusted Close (level)",
     ylab = "log(VIX)", xlab = "", col = "firebrick", lwd = 1.2)

cpi_log_ts <- ts(log(as.numeric(cpi_inf) + 1),   # approximate log level for display
                 start = c(start_yr, start_mo), frequency = 12)
plot(ts(log(fredr(series_id = "CPIAUCNS",
                  observation_start = start_date,
                  observation_end   = end_date)$value),
        start = c(2000, 1), frequency = 12),
     main = "CPI — Log Level",
     ylab = "log(CPI)", xlab = "", col = "steelblue", lwd = 1.2)

par(mfrow = c(1, 1))

# Panel 2: Stationary (differenced) series used in modelling
par(mfrow = c(3, 1), mar = c(3, 4, 3, 1))

plot(CPI_ts, main = "CPI — Monthly Log Change (Inflation Rate, stationary)",
     ylab = "diff(log CPI)", xlab = "", col = "steelblue", lwd = 1.2)
abline(h = 0, lty = 2, col = "gray50")

plot(SP500_ts, main = "S&P 500 — Monthly Log Returns (stationary)",
     ylab = "diff(log Price)", xlab = "", col = "darkgreen", lwd = 1.2)
abline(h = 0, lty = 2, col = "gray50")

plot(VIX_ts, main = "VIX — Monthly Log Changes (stationary)",
     ylab = "diff(log VIX)", xlab = "", col = "firebrick", lwd = 1.2)
abline(h = 0, lty = 2, col = "gray50")

par(mfrow = c(1, 1))



# STEP 3 — AUTOCORRELATIONS AND CROSS-CORRELATIONs

cat("\n=== STEP 3: ACF / PACF / CCF ===\n")

# --- 3a. ACF and PACF for each series ----------------------------------------
par(mfrow = c(3, 2))
acf(CPI_ts,   main = "ACF — CPI Inflation",    lag.max = 36)
pacf(CPI_ts,  main = "PACF — CPI Inflation",   lag.max = 36)
acf(SP500_ts, main = "ACF — S&P 500 Returns",  lag.max = 36)
pacf(SP500_ts,main = "PACF — S&P 500 Returns", lag.max = 36)
acf(VIX_ts,   main = "ACF — VIX",              lag.max = 36)
pacf(VIX_ts,  main = "PACF — VIX",             lag.max = 36)
par(mfrow = c(1, 1))

# --- 3b. Ljung-Box tests for autocorrelation ----------------------------------
lb_cpi   <- Box.test(CPI_ts,   lag = 12, type = "Ljung-Box")
lb_sp500 <- Box.test(SP500_ts, lag = 12, type = "Ljung-Box")
lb_vix   <- Box.test(VIX_ts,   lag = 12, type = "Ljung-Box")

cat("\nLjung-Box Q(12) test for autocorrelation:\n")
cat(sprintf("  CPI   : Q = %.2f, p = %.4f\n", lb_cpi$statistic,   lb_cpi$p.value))
cat(sprintf("  SP500 : Q = %.2f, p = %.4f\n", lb_sp500$statistic, lb_sp500$p.value))
cat(sprintf("  VIX   : Q = %.2f, p = %.4f\n", lb_vix$statistic,   lb_vix$p.value))

# --- 3c. Cross-correlations: CPI → SP500 and CPI → VIX ----------------------
par(mfrow = c(1, 2))
ccf(as.numeric(CPI_ts), as.numeric(SP500_ts),
    lag.max = 24, main = "CCF: CPI → S&P 500 Returns",
    ylab = "CCF", col = "steelblue")
ccf(as.numeric(CPI_ts), as.numeric(VIX_ts),
    lag.max = 24, main = "CCF: CPI → VIX",
    ylab = "CCF", col = "firebrick")
par(mfrow = c(1, 1))

cat("\nSUMMARY — Step 3:\n")
cat("  Inspect ACF/PACF plots for serial structure in each series.\n")
cat("  CCF plots show lead/lag relationships: negative lags = CPI leads output.\n")
cat("  Ljung-Box p < 0.05 indicates significant autocorrelation.\n")


# STEP 4 — SPECTRAL ANALYSIS
cat("\n=== STEP 4: Spectral Analysis ===\n")

# Smoothed periodogram using modified Daniell kernel (bandwidth control)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

spec_cpi   <- spectrum(CPI_ts,   spans = c(3, 5), main = "Smoothed Periodogram — CPI",
                        col = "steelblue", lwd = 1.5)
spec_sp500 <- spectrum(SP500_ts, spans = c(3, 5), main = "Smoothed Periodogram — S&P 500",
                        col = "darkgreen", lwd = 1.5)
spec_vix   <- spectrum(VIX_ts,   spans = c(3, 5), main = "Smoothed Periodogram — VIX",
                        col = "firebrick", lwd = 1.5)
par(mfrow = c(1, 1))

# Dominant frequency for each series
dom_freq_cpi   <- spec_cpi$freq[which.max(spec_cpi$spec)]
dom_freq_sp500 <- spec_sp500$freq[which.max(spec_sp500$spec)]
dom_freq_vix   <- spec_vix$freq[which.max(spec_vix$spec)]

cat(sprintf("\nDominant cycle periods (months):\n"))
cat(sprintf("  CPI   : %.1f months (freq = %.4f)\n", 1/dom_freq_cpi,   dom_freq_cpi))
cat(sprintf("  SP500 : %.1f months (freq = %.4f)\n", 1/dom_freq_sp500, dom_freq_sp500))
cat(sprintf("  VIX   : %.1f months (freq = %.4f)\n", 1/dom_freq_vix,   dom_freq_vix))

cat("\nSUMMARY — Step 4:\n")
cat("  CPI often shows seasonal/annual cycles (~12-month period).\n")
cat("  SP500 returns tend to have flat spectra (close to white noise).\n")
cat("  VIX may show cycles linked to economic stress periods.\n")


# STEP 5 — LINEAR DYNAMIC MODELS AND MODEL COMPARISON

cat("\n=== STEP 5: Linear Dynamic Modelling ===\n")

# Stationarity tests (ADF)
adf_cpi   <- adf.test(CPI_ts)
adf_sp500 <- adf.test(SP500_ts)
adf_vix   <- adf.test(VIX_ts)

cat("\nAugmented Dickey-Fuller stationarity tests:\n")
cat(sprintf("  CPI   : ADF = %.3f, p = %.4f\n", adf_cpi$statistic,   adf_cpi$p.value))
cat(sprintf("  SP500 : ADF = %.3f, p = %.4f\n", adf_sp500$statistic, adf_sp500$p.value))
cat(sprintf("  VIX   : ADF = %.3f, p = %.4f\n", adf_vix$statistic,   adf_vix$p.value))
cat("  (p < 0.05 => reject unit root => stationary)\n")
cat("  Note: SP500 log returns and VIX log changes should be stationary by construction.\n")
cat("  If CPI log changes are not stationary, consider second differencing.\n")


# MODEL A: Univariate ARIMA on S&P 500 returns (baseline)
cat("\n--- Model A: Auto-ARIMA on S&P 500 Returns (baseline, no CPI) ---\n")
modelA <- auto.arima(SP500_ts, seasonal = TRUE, stepwise = FALSE,
                     approximation = FALSE, trace = FALSE)
summary(modelA)
checkresiduals(modelA)   # Ljung-Box + ACF of residuals + histogram


# MODEL B: Transfer Function (dynamic regression)
#          SP500 = f(CPI lagged) + ARIMA errors
cat("\n--- Model B: Transfer Function Model — SP500 ~ CPI (dynamic regression) ---\n")

# Use lagged CPI as external regressor (lag 1 to 3 months)
cpi_lag1 <- stats::lag(CPI_ts, -1)
cpi_lag2 <- stats::lag(CPI_ts, -2)
cpi_lag3 <- stats::lag(CPI_ts, -3)

# Align lags with response (trim NAs)
xreg_B <- cbind(cpi_lag1, cpi_lag2, cpi_lag3)
xreg_B <- window(xreg_B, start = start(SP500_ts) + c(0, 3))
sp500_trim <- window(SP500_ts, start = start(SP500_ts) + c(0, 3))

modelB <- auto.arima(sp500_trim, xreg = xreg_B,
                     seasonal = TRUE, stepwise = FALSE,
                     approximation = FALSE, trace = FALSE)
summary(modelB)
checkresiduals(modelB)


# MODEL C: Transfer Function — VIX log changes ~ CPI
cat("\n--- Model C: Transfer Function Model — VIX Log Changes ~ CPI ---\n")
vix_trim <- window(VIX_ts, start = start(VIX_ts) + c(0, 3))

modelC <- auto.arima(vix_trim, xreg = xreg_B,
                     seasonal = TRUE, stepwise = FALSE,
                     approximation = FALSE, trace = FALSE)
summary(modelC)
checkresiduals(modelC)



# MODEL D: VAR model (SP500 + VIX + CPI jointly)

cat("\n--- Model D: VAR Model (SP500, VIX, CPI jointly) ---\n")

var_data <- cbind(SP500 = SP500_ts, VIX = VIX_ts, CPI = CPI_ts)
# Select optimal lag order by AIC
lag_sel <- VARselect(var_data, lag.max = 12, type = "const")
cat("\nVAR lag selection (AIC):\n")
print(lag_sel$criteria)
optimal_lag <- lag_sel$selection["AIC(n)"]
cat(sprintf("Optimal lag by AIC: %d\n", optimal_lag))

modelD <- VAR(var_data, p = optimal_lag, type = "const")
summary(modelD)

# VAR residual diagnostics
cat("\nVAR serial correlation test (Portmanteau):\n")
print(serial.test(modelD, lags.pt = 12, type = "PT.asymptotic"))

# Granger causality: does CPI Granger-cause SP500?
cat("\nGranger causality — CPI → SP500:\n")
print(causality(modelD, cause = "CPI")$Granger)

cat("\nGranger causality — CPI → VIX:\n")
print(causality(modelD, cause = "CPI")$Granger)

# Impulse Response Functions
irf_sp500 <- irf(modelD, impulse = "CPI", response = "SP500",
                  n.ahead = 18, boot = TRUE, ci = 0.95)
irf_vix   <- irf(modelD, impulse = "CPI", response = "VIX",
                  n.ahead = 18, boot = TRUE, ci = 0.95)

par(mfrow = c(1, 2))
plot(irf_sp500, main = "IRF: CPI shock → S&P 500")
plot(irf_vix,   main = "IRF: CPI shock → VIX")
par(mfrow = c(1, 1))

# MODEL COMPARISON TABLE (AIC / BIC)
cat("\n--- Model Comparison ---\n")
aic_vals <- c(ModelA_ARIMA_SP500   = AIC(modelA),
              ModelB_TF_SP500_CPI  = AIC(modelB),
              ModelC_TF_VIX_CPI    = AIC(modelC))

bic_vals <- c(ModelA_ARIMA_SP500   = BIC(modelA),
              ModelB_TF_SP500_CPI  = BIC(modelB),
              ModelC_TF_VIX_CPI    = BIC(modelC))

comparison <- data.frame(AIC = round(aic_vals, 2),
                         BIC = round(bic_vals, 2))
print(comparison)
cat("  Lower AIC/BIC => better model.\n")
cat("  If Model B < Model A => CPI improves SP500 prediction (supports H1).\n")
cat("  If Model C residuals are white noise => CPI explains VIX well (supports H2).\n")


# RESIDUAL WHITE NOISE CHECK (Ljung-Box on all models)
cat("\n--- Ljung-Box on residuals (H0: white noise) ---\n")
resid_tests <- list(ModelA = residuals(modelA),
                    ModelB = residuals(modelB),
                    ModelC = residuals(modelC))

for (nm in names(resid_tests)) {
  lb <- Box.test(resid_tests[[nm]], lag = 12, type = "Ljung-Box")
  cat(sprintf("  %s: Q(12) = %.2f, p = %.4f %s\n",
              nm, lb$statistic, lb$p.value,
              ifelse(lb$p.value > 0.05, "(white noise ✓)", "(NOT white noise ✗)")))
}



# STEP 6 — CONCLUSIONS

cat("\n=== STEP 6: CONCLUSIONS ===\n")
cat("
HYPOTHESES REVISITED:
---------------------
H1 (CPI → S&P 500 returns):
  - Compare AIC of Model A vs Model B.
  - If Model B has lower AIC and CPI lag coefficients are significant,
    inflation has a measurable lagged effect on equity returns.
  - IRF from VAR shows direction and duration of the CPI shock on SP500.
  - Granger causality test provides formal statistical evidence.

H2 (CPI → VIX):
  - Model C tests whether CPI lags explain VIX.
  - If coefficients on CPI lags are positive and significant, rising
    inflation leads to higher implied volatility (increased market uncertainty).
  - The IRF for CPI → VIX reveals whether the effect is immediate or delayed.

H3 (Lagged transmission):
  - The significant lags in transfer function models and the IRF horizon
    indicate how many months it takes CPI changes to propagate into markets.
  - Policy transmission is typically 3–12 months per economic literature.

MODELLING CONCLUSIONS:
----------------------
- If residuals from the best model pass the Ljung-Box test (p > 0.05),
  the model has adequately captured the serial structure.
- VAR provides a comprehensive system view — Granger causality tests are
  the cleanest formal test of the directional hypotheses.
- Model comparison via AIC/BIC determines whether adding CPI as an input
  improves predictive performance beyond a univariate ARIMA baseline.

LIMITATIONS:
------------
- CPI is released with a ~1-month lag; daily market data may already
  price in inflation expectations before the official print.
- VIX log changes (diff of log VIX) are used for stationarity; interpret
  coefficients as the effect of CPI on the *rate of change* of volatility.
- Structural breaks (2008 GFC, 2020 COVID, 2022 rate hikes) may
  violate parameter stability — consider adding dummy variables or
  estimating over sub-periods.
- SP500 adjusted close corrects for dividends/splits; log returns are
  continuously compounded returns and directly comparable across time.
")

cat("\n=== SCRIPT COMPLETE ===\n")
