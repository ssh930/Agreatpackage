#'Alpha and Beta of Portfolio
#'
#'@param tickers This should be a string or a vector of strings. It is the tickers of stocks you want to look at.
#'
#'@param wts This should be a double or a vector of doubles. It is the weights for each according ticker in your portfolio.
#'
#'@param from This should be a string in form of 'yyyy-mm-dd'. It is the start of the time interval of your portfolio.
#'
#'@param to This should be a string in form of 'yyyy-mm-dd'. It is the end of the time interval of your portfolio.
#'
#'@param bench This should be a string. It is the ticker of the bench stock which is considered as the expected return.
#'
#'@param free This should be a double. It is the risk-free interest rate.
#'
#'@examples
#'alp_bet(c('JNJ' , 'WFC' , 'KO') , c(0.4 , 0.4 , -0.2) , '2020-11-11' , '2020-12-11' , 'SPX' , 0.09)


alp_bet <- function(ticker , wts , from , to , bench , free){
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

  bench_price <- tq_get('SPY', from = from , to = to , get = 'stock.prices')

  bench_ret <- bench_price %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "bench_ret")

  comb_ret <- left_join(port_ret - free/252,
                        bench_ret - free/252,
                        by = 'date')

  comb_ret
  model <- lm(comb_ret$port_ret ~ comb_ret$bench_ret)

  model_alpha <- model$coefficients[1]

  model_beta <- model$coefficients[2]

  cat("The portfolio alpha is", model_alpha, "and the portfolio beta is", model_beta)
}