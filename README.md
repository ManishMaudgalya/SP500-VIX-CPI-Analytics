Market Time Series Analysis
This repository analyzes the relationships between the S&P 500 (SPX), VIX, and CPI using classical and modern time‑series techniques. The project focuses on understanding how equity performance, market volatility, and inflation interact over time through statistical modeling and exploratory analysis.

Project Overview
Collect and preprocess historical SPX, VIX, and CPI data

Explore trends, seasonality, and stationarity across series

Apply ARIMA, VAR, and state‑space models

Evaluate cross‑correlations and lagged effects

Visualize macro‑financial interactions and volatility dynamics

Methods
Time‑series decomposition

Correlation and Granger causality tests

ARIMA / SARIMA modeling

Vector Autoregression (VAR)

Forecasting and residual diagnostics

Goals
Quantify how volatility (VIX) and inflation (CPI) influence SPX movements

Compare model performance across indicators

Build interpretable insights into market regimes and macro linkages

Repository Structure
Code
/data          # Raw and processed datasets
/notebooks     # Exploratory analysis and modeling workflows
/models        # Saved model objects and outputs
/plots         # Visualizations and diagnostics
src/           # Utility functions and helpers
Future Enhancements
Add ML‑based forecasting models

Incorporate additional macroeconomic indicators

Develop an interactive dashboard for visualization
