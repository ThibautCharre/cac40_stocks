# ------------------------------------
# FUNCTION : get_hist
# ------------------------------------
# Function to extract a company stock historical value from Yahoo Finance
# ------------------------------------
# @parameters
# - symbol: company ticker
# - nb_month : nb month to determine start date
# - interval : interval of data points
# ------------------------------------
get_hist <- function(symbol, nb_month = 36, interval='1d') {
  
  # URL definition
  path <- "v8/finance/chart/"
  end_point <- paste0(path, symbol)
  url <- paste0("https://query1.finance.yahoo.com/", end_point)
  
  # Dates and list query selection
  end_date <- round(as.numeric(as.POSIXct(Sys.Date() - 1)))
  start_date <- round(as.numeric(as.POSIXct(Sys.Date() - 365.25 * nb_month / 12)))
  qlist <- list(period1 = start_date, period2 = end_date, interval = interval)
  
  # Response of the server and data formating
  resp <- GET(url, query = qlist)
  
  # If error 404 we return a NA
  if (resp$status_code == 404) {
    result <- NA_integer_
  } else {
    
    parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), 
                       simplifyVector = FALSE)
    data <- parsed %>% use_series(chart) %>% use_series(result) %>% 
      extract2(1)
    # indicators <- data %>% use_series(indicators) %>% use_series(quote) %>% 
    #   extract2(1)
    adj_close <- data %>% use_series(indicators) %>% use_series(adjclose) %>% 
      extract2(1) %>% use_series(adjclose)
    adj_close[sapply(adj_close, is.null)] <- NA_real_
    
    result <- data.frame(date = as.Date(as_datetime(unlist(data$timestamp)), "%Y-%m-%d"), 
                         close = round(unlist(adj_close), 2))
  }
  return(result)
}

# ------------------------------------
# FUNCTION : get_hist_graph
# ------------------------------------
# Function to display historical values of a company stock
# ------------------------------------
# @parameters
# - list_stocks : a list of stocks values generated when app is launched
# - symbol: company ticker
# - start_date
# - end_date
# ------------------------------------
get_value_graph <- function(list_stocks, symbol, valueType="close", showAverage = TRUE, start_date=NULL, end_date=NULL) {
  
  # DT of the stock selected
  stockDT <- as.data.table(list_stocks[[symbol]]) %>% .[, .(date, get(valueType))]
  colnames(stockDT) <- c("date", valueType)
  stockDT$date <- as.Date(stockDT$date, format = "%Y-%m-%d")
  
  # If NA value we take the preceding value but highlight it
  stockDT[, rk := .I]
  indxNA <- stockDT[is.na(get(valueType)), rk]
  preValues <- stockDT[indxNA - 1, get(valueType)]
  stockDT[indxNA, valueType] <- preValues
  
  # We calculate moving average price
  if (showAverage) {
    for (i in c(20, 50, 100, 200)) {
      # 20 days
      stockDT[, paste0("mavg", i) :=   sapply(stockDT$rk, function(x) {
        if ((x - i) > 0) {
          mean(stockDT[(x-i):x, get(valueType)])
        } else {
          NA_real_
        }
      })]
    }
  }
  
  # Filter on dates
  if (!is.null(start_date) & !is.null(end_date)) {
    start_date <- max(min(stockDT$date), start_date)
    end_date <- min(max(stockDT$date), end_date)
    stockDT <- stockDT[date >= start_date & date <= end_date]
  }
  
  # plotly graph
  fig <- plot_ly(data = stockDT, name = "Stock Price", type = "scatter", mode = "lines",
                 x=~date, y=~get(valueType, pos = stockDT),
                 line = list(width = 4),
                 hovertemplate = ~paste0("Date : <b>", date, 
                                         "<br></b>Stock Value : <b>", round(get(valueType, pos = stockDT), 2), " €",
                                         "<extra></extra>"))
  
  if (showAverage) {
    
    
    fig <- fig %>% 
      add_trace(name = "20d Moving Average", mode = "lines", type = "scatter", 
                y = ~mavg20, line = list(width = 1), 
                hovertemplate = ~paste0("Date : <b>", date, 
                                        "<br></b>20d Mean : <b>", round(mavg20, 2), " €",
                                        "<extra></extra>")) %>%
      add_trace(name = "50d Moving Average", mode = "lines", type = "scatter", 
                y = ~mavg50, line = list(width = 1), 
                hovertemplate = ~paste0("Date : <b>", date, 
                                        "<br></b>50d Mean : <b>", round(mavg50, 2), " €",
                                        "<extra></extra>")) %>%
      add_trace(name = "100d Moving Average", mode = "lines", type = "scatter", 
                y = ~mavg100, line = list(width = 1),
                hovertemplate = ~paste0("Date : <b>", date, 
                                        "<br></b>100d Mean : <b>", round(mavg100, 2), " €",
                                        "<extra></extra>")) %>%
      add_trace(name = "200d Moving Average", mode = "lines", type = "scatter", 
                y = ~mavg200, line = list(width = 1), 
                hovertemplate = ~paste0("Date : <b>", date, 
                                        "<br></b>200d Mean : <b>", round(mavg200, 2), " €",
                                        "<extra></extra>"))
    
  }
  
  fig <- layout(p = fig, 
                title = paste0(symbol,  " -  Historical Stock Value"),
                showlegend = TRUE,
                xaxis = list(title="Date"),
                yaxis = list(title = "Value"),
                plot_bgcolor = "rgba(255, 255, 255, 0)", 
                paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  fig
  
}

# ------------------------------------
# FUNCTION : get_return_distr
# ------------------------------------
# Function to display historical returns of a company stock
# ------------------------------------
# @parameters
# - list_stocks : a list of stocks values generated when app is launched
# - symbol: company ticker
# - valueType
# - nbDaysReturn: time interval for return calc
# - start_date
# - end_date
# ------------------------------------
get_return_distr <- function(list_stocks, symbol, valueType="close", nbDaysReturn = 1, start_date=NULL, end_date=NULL) {
  
  # DT of the stock selected
  stockDT <- as.data.table(list_stocks[[symbol]]) %>% .[, .(date, get(valueType))]
  colnames(stockDT) <- c("date", valueType)
  stockDT$date <- as.Date(stockDT$date, format = "%Y-%m-%d")
  
  # Filter on dates
  if (!is.null(start_date) & !is.null(end_date)) {
    start_date <- max(min(stockDT$date), start_date)
    end_date <- min(max(stockDT$date), end_date)
    stockDT %<>% .[date >= start_date & date <= end_date]
  }
  
  # If NA value we take the preceding value but highlight it
  stockDT[, rk := .I]
  indxNA <- stockDT[is.na(get(valueType)), rk]
  preValues <- stockDT[indxNA - 1, get(valueType)]
  stockDT[indxNA, valueType] <- preValues
  
  # We calculate daily returns
  stockDT[, ':=' (lag1=shift(x = get(valueType), n = nbDaysReturn))]
  stockDT[, ':=' (return1=round(100 * log(get(valueType)/lag1), 2))] 
  stockDT %<>% .[!is.na(return1)]
  
  # Mean and Median calculation
  meanC <- round(mean(stockDT$return1), 2)
  medianC <- round(median(stockDT$return1), 2)
  sdC <- round(sd(stockDT$return1), 2)
  
  # VaR, ES95, ES99 calculations
  stockDTvar <- stockDT[!is.na(return1)][order(-return1)]
  nb_data <- nrow(stockDTvar)
  
  var95 <- round(stockDTvar[ceiling(nb_data * 0.95), return1], 2)
  var99 <- round(stockDTvar[ceiling(nb_data * 0.99), return1], 2)
  es95 <- round(mean(stockDTvar[ceiling(nb_data * 0.95):nb_data, return1]), 2)
  es99 <- round(mean(stockDTvar[ceiling(nb_data * 0.99):nb_data, return1]), 2)
  
  # Kurtosis and Skewness calculations
  skewC <- round(1/nb_data * (sum(stockDTvar$return1 - mean(stockDTvar$return1))/sdC)^3, 2) 
  kurtoC <- round(1/nb_data * (sum(stockDTvar$return1 - mean(stockDTvar$return1))/sdC)^4, 2)
  
  # Final data table filled with stats
  statDT <- data.table(Mean = paste0(meanC, "%"), Median = paste0(medianC, "%"), Volatility = paste0(sdC, "%"), 
                       "VaR 95%" = paste0(var95, "%"), "ES 95%" = paste0(es95, "%"), 
                       Skewness = skewC, Kurtosis = kurtoC)
  
  # Data for viz efficiency
  stockDTgraph <- copy(stockDT)
  stockDTgraph[, roundedRet := floor(return1)]
  stockDTgraph <- stockDTgraph[, .(Total=.N), by = .(roundedRet)]
  maxHigh <- max(stockDTgraph$Total)
  
  # Plotly histogram
  fig <- plot_ly() %>%
    # add_trace(type = "bar", name = "Daily Returns Distribution", x = ~roundedRet, y = ~Total, 
    #           marker = list(color = "rgba(60, 179, 113, 0.6)", line = list(color = "rgba(8, 48, 107, 0.6)", width = 1.5)), 
    #           hovertemplate = "") %>%
    add_trace(data = stockDT, type = "histogram", x = ~return1, name = "Daily Returns Distribution",
              xbins = list(size = 1),
              marker = list(color = "rgba(60, 179, 113, 0.6)", line = list(color = "rgba(8, 48, 107, 0.6)", width = 1.5)), 
              hovertemplate = "") %>%
    add_lines(data = stockDTgraph, x = c(meanC, meanC), y = c(0, maxHigh), name = "1d Return Mean",
              line = list(color = "rgb(106, 90, 205)", width = 2), 
              hovertemplate = "") %>%
    add_lines(data = stockDTgraph, x = c(medianC, medianC), y = c(0, maxHigh), name = "1d Return Median", 
              line = list(color = "rgb(0, 0, 255)", width = 2), 
              hovertemplate = "") %>%
    add_lines(data = stockDTgraph, x = c(var95, var95), y = c(0, maxHigh), name = "H-VaR 95%",
              line = list(color = "rgb(90, 90, 90)", width = 2), 
              hovertemplate = "") %>%
    add_lines(data = stockDTgraph, x = c(var99, var99), y = c(0, maxHigh), name = "H-VaR 99%",
              line = list(color = "rgb(60, 60, 60)", width = 2), 
              hovertemplate = "") %>%
    layout(bargap = 0, title = paste0(symbol, " - ", "Daily Returns Distribution"),
           xaxis = list(title = "Daily Returns", ticksuffix = "%", tickangle = 45, 
                        dtick = 2, ticklen = 5, ticks = "outside", showgrid = TRUE), 
           yaxis = list(title = "Total Number", range = c(0, maxHigh + 2)), 
           plot_bgcolor = "rgba(255, 255, 255, 0)", 
           paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  return(list(Graph=fig, statDT=statDT))
  
}

# ------------------------------------
# FUNCTION : get_spread_stock_index
# ------------------------------------
# Function to display spread curves
# ------------------------------------
# @parameters
# - list_stocks : a list of stocks values generated when app is launched
# - symbol: company ticker
# - valueType
# - nbDaysReturn: time interval for return calc
# - start_date
# - end_date
# ------------------------------------
get_spread_stock_index <- function(list_stocks, symbol, valueType = "close", start_date = NULL, end_date = NULL) {
  
  # Stock selected to DT
  stockDT <- as.data.table(list_stocks[[symbol]])
  
  # If NA value we take the preceding value but highlight it
  stockDT[, rk := .I]
  indxNA <- stockDT[is.na(get(valueType)), rk]
  preValues <- stockDT[indxNA - 1, get(valueType)]
  stockDT[indxNA, valueType] <- preValues
  stockDT <- stockDT[, .(date, get(valueType))]
  
  # Index values
  indexDT <- as.data.table(list_stocks[["^FCHI"]])
  indexDT <- indexDT[, .(date, get(valueType))] 
  
  # Filter on dates
  if (!is.null(start_date) & !is.null(end_date)) {
    start_date <- max(min(stockDT$date), start_date)
    end_date <- min(max(stockDT$date), end_date)
    stockDT %<>% .[date >= start_date & date <= end_date]
    indexDT %<>% .[date >= start_date & date <= end_date]
  }
  
  # We merge datas
  spreadDT <- merge(indexDT, stockDT, by = "date", all = TRUE)
  colnames(spreadDT) <- c("date", "Index", symbol)
  
  # Normalization by dividing with value at t=0
  for (name in c("Index", symbol)) {
    val0 <- spreadDT[1, get(name)]
    spreadDT[, paste0(name, "_norm") := round(get(name)/val0, 2)]
  }
  
  spreadDT[, spread := -get("Index_norm") + get(paste0(symbol, "_norm"))]
  
  # plotly graph
  fig <- plot_ly(data = spreadDT, type = "scatter", mode = "lines") %>%
    add_trace(x = ~date, y = ~Index_norm, name = "Index",
              line = list(color = "green", width = 2), fillcolor = "green", 
              hovertemplate = "") %>%
    add_trace(x = ~date, y = ~get(paste0(symbol, "_norm")), name = symbol,
              line = list(color = "blue", width = 2), 
              hovertemplate = "")
  
  fig <- layout(p = fig, 
                title = paste0(symbol, "vs. Index Performances"),
                yaxis = list(title = "Normalized Value (V(t)/V(t=0)"),
                xaxis = list(title = "Date"))
  
  fig
  
}

# ------------------------------------
# FUNCTION : get_compared_stocks
# ------------------------------------
# Function to compare multiple stocks
# ------------------------------------
# @parameters
# - list_stocks : a list of stocks values generated when app is launched
# - symbol: company ticker
# - valueType
# - start_date
# - end_date
# ------------------------------------
get_compared_stocks <- function(list_stocks, symbol, valueType = "close", start_date = NULL, end_date = NULL) {
  
  # List gathering DTs of selected symbols
  listStocks <- lapply(symbol, function(x) {
    
    dtTMP <- list_stocks[[x]] %>%
      as.data.table() %>%
      .[, .(date, get(valueType))]
    colnames(dtTMP) <- c("date", valueType)
    
    # If NA value we take the preceding value but highlight it
    if (nrow(dtTMP[is.na(get(valueType))]) > 0) {
      dtTMP[, rk := .I]
      indxNA <- dtTMP[is.na(get(valueType)), rk]
      preValues <- dtTMP[indxNA - 1, get(valueType)]
      dtTMP[indxNA, valueType] <- preValues
      dtTMP[, rk := NULL]
    }
    
    dtTMP
    
  })
  
  # Name of list elements
  names(listStocks) <- symbol
  
  # Binding of list and dcast to display stocks by columns
  fullDT <- rbindlist(listStocks, idcol = "Stock")
  fullDT <- dcast(data = fullDT, formula = date ~ Stock, value.var = valueType)
  
  # # Return of a correlation plot
  corrMatrix <- cor(fullDT[, 2:ncol(fullDT)])
  
  # DT output with returns and volatility for each stock
  for (name_stock in (symbol)) {
    fullDT[, paste0(name_stock, "_return1d") := round(100 * log(fullDT[, get(name_stock)] / shift(fullDT[, get(name_stock)], 1)), 2)]
  }
  returnVect <- sapply(symbol, function(x) {
    round(mean(fullDT[, get(paste0(x, "_return1d"))], na.rm=T), 2)
  })
  volVect <- sapply(symbol, function(x) {
    round(sd(fullDT[, get(paste0(x, "_return1d"))], na.rm=T), 2)
  })  
  perfVect <- sapply(symbol, function(x) {
    round(100 * (fullDT[nrow(fullDT), get(x)] - fullDT[1, get(x)]) / fullDT[1, get(x)], 2)
  })
  sumDT <- data.table(symbol = symbol, "Mean Returns" = returnVect, 
                      volatility = volVect, "Sharpe Ratio" = round(100 * returnVect / volVect, 2), 
                      Performance = perfVect)
  
  # melt of DT and plot of returns on histogram or density graph 
  fullDT2 <- fullDT[, .SD, .SDcols = c("date", colnames(fullDT)[grepl("return1d", colnames(fullDT))])] %>%
    melt(id.vars = c("date"), variable.name = "stocks")
  
  fig <- qplot(x = value, data = fullDT2, geom = "density", fill = stocks, alpha=I(.4)) %>%
    ggplotly(tooltip = "") %>%
    layout(title = "Distribution of Daily Returns", 
           xaxis = list(title = "Returns (%)", ticksuffix = "%", tickangle = 45, dtick = 2, 
                        showgrid = T), 
           yaxis = list(title = "Density"),
           plot_bgcolor = "rgba(255, 255, 255, 0.5)")
  
  # List object to be returned
  return(list(densityGraph = fig, sumDT = sumDT, corrMatrix = corrMatrix))
  
}

# ------------------------------------
# FUNCTION : get_compared_stocks
# ------------------------------------
# Function to compare multiple stocks
# ------------------------------------
# @parameters
# - list_stocks : a list of stocks values generated when app is launched
# - symbol: company ticker
# - valueType
# - start_date
# - end_date
# ------------------------------------
get_arima <- function(list_stocks, symbol, valueType = "close", start_date = NULL, end_date = NULL) {
  
  # DT of the stock selected
  stockDT <- as.data.table(list_stocks[[symbol]]) %>% 
    .[, .(date, get(valueType))]
  colnames(stockDT) <- c("date", valueType)
  stockDT$date <- as.Date(stockDT$date, format = "%Y-%m-%d")
  
  # If NA value we take the preceding value but highlight it
  if (nrow(stockDT[is.na(get(valueType)), .(get(valueType))]) > 0) {
    stockDT[, rk := .I]
    indxNA <- stockDT[is.na(get(valueType)), rk]
    preValues <- stockDT[indxNA - 1, get(valueType)]
    stockDT[indxNA, valueType] <- preValues
  }
  
  # Log value of price (easier for differenciation)
  stockDT[, value_log := log(get(valueType))]
  stockDT[, diff := c(NA_real_, diff(stockDT$value_log, lag = 1))]
  stockDT %<>% .[!is.na(diff)]
  
  # Augmented Dickey Fulley test
  adf <- adf.test(stockDT$diff, alternative = "stationary", k = 0)
  p_val <- adf$p.value
  
  if (p_val < 0.05) {
    # Decomposition of training data and test data
    training_percent = .8
    training_data <- stockDT[1:floor(training_percent * nrow(stockDT)), "diff"]
    test_data <- stockDT[(floor(training_percent * nrow(stockDT))+1):nrow(stockDT), "diff"]
    
    # ARIMA model
    arima_model <- auto.arima(y = training_data, stationary = T)
    p_arima <- arima_model$arma[1]
    q_arima <- arima_model$arma[2]
    
    # Calculation of error percentage over the test datas
    arima_err <- arima(training_data, order = c(p_arima, 0, q_arima))
    arima_err_forecast <- forecast(arima_err, h = nrow(test_data))
    error_prev <- round(100 * mean((test_data$diff - arima_err_forecast$mean)^2), 2)
    
    # Ultime arima model for next n days
    arima_full <- arima(stockDT$diff, order = c(p_arima, 0, q_arima))
    forecast_full <- forecast(arima_full, h = 100)
    
    # DT of future values
    valuesDT <- data.table(date = seq(1, length(stockDT$date) + 100, 1), 
                           returns = c(stockDT$diff, forecast_full$mean), 
                           low95 = c(stockDT$diff, forecast_full$lower[, "95%"]), 
                           high95 = c(stockDT$diff, forecast_full$upper[, "95%"]), 
                           close = c(stockDT$close, rep(NA_real_, 100)))
    
    for (idx in seq(length(stockDT$date), length(stockDT$date) + 100, 1)) {
      future_price <- valuesDT[idx - 1, "close"] * exp(valuesDT[idx, "returns"])
      future_price_low95 <- valuesDT[idx - 1, "close"] * exp(valuesDT[idx, "low95"])
      future_price_high95 <- valuesDT[idx - 1, "close"] * exp(valuesDT[idx, "high95"])
    }
    
    valuesDT
    
  }
  
}









