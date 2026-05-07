# Market Dynamics Analysis: S&P 500, VIX, and Inflation (2010-2023)
library(quantmod)
library(tseries)
library(forecast)
library(vars)  
library(astsa)
library(urca)
library(rugarch)
library(strucchange)
library(zoo) 


#DATA IMPORTATION & ALIGNMENT
print("Pulling data from Yahoo Finance and FRED...")

# Pull S&P 500 and VIX monthly
getSymbols("^GSPC", from = "2010-01-01", to = "2023-12-31", periodicity = "monthly")
getSymbols("^VIX", from = "2010-01-01", to = "2023-12-31", periodicity = "monthly")

# Pull CPI from FRED
getSymbols("CPIAUCSL", src = "FRED")


sp500_raw <- GSPC[, 6]  # Adjusted Close
vix_raw   <- VIX[, 4]   # Close
cpi_raw   <- CPIAUCSL

raw_data <- na.omit(merge(sp500_raw, vix_raw, cpi_raw))
colnames(raw_data) <- c("SP500_Level", "VIX_Level", "CPI_Level")


#TRANSFORMATIONS & STATIONARITY
log_data <- log(raw_data)

stat_data <- na.omit(diff(log_data))
colnames(stat_data) <- c("SP500_Ret", "VIX_Ret", "CPI_Inf")

roll_corr <- rollapply(stat_data, width = 24, 
                       FUN = function(x) cor(x[,1], x[,2]), 
                       by.column = FALSE, align = "right")

plot(roll_corr, main = "24-Month Rolling Correlation: S&P 500 vs VIX", 
     ylab = "Correlation", xlab = "Date", col = "darkblue", lwd = 2)
abline(h = mean(roll_corr, na.rm=TRUE), col="red", lty=2)


sq_returns <- stat_data$SP500_Ret^2
plot(sq_returns, main = "Realized Variance (Squared S&P 500 Returns)",
     ylab = "Squared Returns", col = "darkred", type = "l")


#SPECTRAL ANALYSIS & CROSS-CORRELATIONS
ccf(as.numeric(stat_data$CPI_Inf), as.numeric(stat_data$SP500_Ret),
    lag.max = 24, main = "CCF: CPI -> S&P 500 Returns")

# Periodogram
spec_sp500 <- spectrum(as.numeric(stat_data$SP500_Ret), spans = c(3, 5), main="S&P 500 Spectrum")
spec_vix   <- spectrum(as.numeric(stat_data$VIX_Ret), spans = c(3, 5), main="VIX Spectrum")


# COINTEGRATION TEST
johansen_test <- ca.jo(log_data, type = "trace", ecdet = "const", K = 4)
print("Johansen Cointegration Test:")
summary(johansen_test) 


# 5. DYNAMIC LINEAR MODELS (SARIMAX & VAR)
# Baseline Model: Univariate ARIMA for S&P 500
model_baseline <- auto.arima(stat_data$SP500_Ret, seasonal = TRUE)

# Contemporaneous SARIMAX: Using current month VIX and CPI as inputs
xreg_contemp <- cbind(VIX = stat_data$VIX_Ret, CPI = stat_data$CPI_Inf)
model_contemp <- auto.arima(stat_data$SP500_Ret, xreg = xreg_contemp, seasonal = TRUE)

print("AIC Comparison: Baseline vs Contemporaneous ARIMAX")
print(c(Baseline_AIC = model_baseline$aic, Contemp_AIC = model_contemp$aic))

# Vector Autoregression (VAR) framework
# Finding the optimal lag based on AIC
var_input <- cbind(stat_data$SP500_Ret, stat_data$VIX_Ret, stat_data$CPI_Inf)
colnames(var_input) <- c("SP500_Ret", "VIX_Ret", "CPI_Inf") # Explicitly forcing names
lag_sel <- VARselect(var_input, lag.max = 12, type = "const")
optimal_lag <- lag_sel$selection["AIC(n)"] 

# Fit the VAR model
model_var <- VAR(var_input, p = optimal_lag, type = "const")

# Granger Causality
causality_test <- causality(model_var, cause = "CPI_Inf")
print("Granger Causality: CPI causing system?")
print(causality_test$Granger)

# Impulse Response Function
irf_sp500 <- irf(model_var, impulse = "CPI_Inf", response = "SP500_Ret",
                 n.ahead = 18, boot = TRUE, ci = 0.95)
plot(irf_sp500, main = "VAR IRF: Shock to CPI -> S&P 500 Response")

# 6. VOLATILITY MODELING (GARCH)
# ARMA(1,1)-GARCH(1,1) 
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, 
                    external.regressors = as.matrix(stat_data$CPI_Inf)),
  distribution.model = "std" 
)

print("Fitting GARCH(1,1) Model")
garch_fit <- ugarchfit(spec = garch_spec, data = stat_data$SP500_Ret)
show(garch_fit)


#STRUCTURAL BREAKS Regime Changes
ols_model <- SP500_Ret ~ VIX_Ret + CPI_Inf
breakpoints_model <- breakpoints(ols_model, data = as.data.frame(stat_data), h = 0.15)

print("Structural Breakpoints:")
summary(breakpoints_model)

# Plot the S&P 500 returns 
plot(stat_data$SP500_Ret, main = "S&P 500 Returns with Detected Structural Breaks")
lines(breakpoints_model, col = "red", lwd = 2)
