library(tidyquant)
library(ggplot2)
library(dplyr)
library(GGally)


tickers <- c("GOOG", "JNJ", "WMT", "META", "JPM")

returns_data <- tq_get(tickers,
                       get = "stock.prices",
                       from = "2015-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "monthly_return") %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = symbol, values_from = monthly_return)

returns_data_clean <- na.omit(returns_data)



ggpairs(returns_data_clean[, -1],
        title = "Monthly Return Correlation Matrix")



goog <- tq_get("GOOG", get = "stock.prices", from = "2015-01-01") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "goog_return")

sp500 <- tq_get("^GSPC", get = "stock.prices", from = "2015-01-01") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "market_return")

returns <- left_join(goog, sp500, by = "date")

rf_annual <- 0.0432
rf_monthly <- rf_annual / 12

returns <- returns %>%
  mutate(excess_goog = goog_return - rf_monthly,
         excess_market = market_return - rf_monthly)

sharpe_ratio <- mean(returns$excess_goog, na.rm = TRUE) / sd(returns$goog_return, na.rm = TRUE)

model <- lm(excess_goog ~ excess_market, data = returns)
alpha <- coef(model)[1]
beta <- coef(model)[2]
jensen_alpha_annualized <- (1 + alpha)^12 - 1

