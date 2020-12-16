#'Sharpe Ratio of the Portfolio
#'
#'@param tickers This should be a string or a vector of strings. It is the tickers of stocks you want to look at.
#'
#'@param wts This should be a double or a vector of doubles. It is the weights for each according ticker in your portfolio.
#'
#'@param from This should be a string in form of 'yyyy-mm-dd'. It is the start of the time interval of your portfolio.
#'
#'@param to This should be a string in form of 'yyyy-mm-dd'. It is the end of the time interval of your portfolio.
#'
#'@param free This should be a double. It is the risk-free interest rate.
#'
#'@examples
#'sha(c('JNJ' , 'WFC' , 'KO') , c(0.4 , 0.4 , -0.2) , '2020-11-11' , '2020-12-11' , 0.09)

library(tidyquant)

sha <- function(ticker , wts , from , to , free){
  price_data <- tq_get(tickers , from = from , to = to , get = 'stock.prices')

  ret_data <- price_data %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "ret")

  wts_tbl <- tibble(symbol = tickers,
                    wts = wts)
  ret_data <- left_join(ret_data,wts_tbl, by = 'symbol')

  ret_data <- ret_data %>%
    mutate(wt_return = wts * ret)

  port_ret <- ret_data %>%
    tq_portfolio(assets_col = symbol,
                 returns_col = ret,
                 weights = wts,
                 col_rename = 'port_ret',
                 geometric = FALSE)

  sharpe_ratio <- port_ret %>%
    tq_performance(Ra = port_ret,
                   performance_fun = SharpeRatio.annualized,
                   Rf = 0.009/252) %>%
    .[[1]]

  cat("The annual portfolio sharpe ratio calculated using the tq_performance function is", round((sharpe_ratio),4))
}
