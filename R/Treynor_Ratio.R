#'Treynor Ratio of Portfolio
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
#'tre(c('JNJ' , 'WFC' , 'KO') , c(0.4 , 0.4 , -0.2) , '2020-11-11' , '2020-12-11' , 'SPY' , 0.09)
#'@importFrom tidyquant tq_get
#'@importFrom tidyquant tq_transmute
#'@importFrom tidyquant tq_portfolio
#'@importFrom dplyr tibble
#'@importFrom dplyr left_join
#'@importFrom dplyr mutate
#'@importFrom dplyr group_by
#'@importFrom magrittr "%>%"
#'@importFrom stats lm
#'@export
tre <- function(tickers , wts , from , to , bench , free){
  price_data <- tidyquant::tq_get(tickers , from = from , to = to , get = 'stock.prices')

  ret_data <- price_data %>%
    dplyr::group_by(symbol) %>%
    tidyquant::tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "ret")

  wts_tbl <- dplyr::tibble(symbol = tickers,
                    wts = wts)
  ret_data <- dplyr::left_join(ret_data,wts_tbl, by = 'symbol')

  ret_data <- ret_data %>%
    dplyr::mutate(wt_return = wts * ret)

  port_ret <- ret_data %>%
    tidyquant::tq_portfolio(assets_col = symbol,
                 returns_col = ret,
                 weights = wts,
                 col_rename = 'port_ret',
                 geometric = FALSE)

  average_annual_port_ret <- port_ret %>%
    tidyquant::tq_performance(Ra = port_ret,
                   performance_fun = Return.annualized)

  bench_price <- tq_get(bench, from = from , to = to , get = 'stock.prices')

  bench_ret <- bench_price %>%
    tidyquant::tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "bench_ret")

  comb_ret <- dplyr::left_join(port_ret - free/252,
                        bench_ret - free/252,
                        by = 'date')

  comb_ret
  model <- lm(comb_ret$port_ret ~ comb_ret$bench_ret)

  model_beta <- model$coefficients[2]

  model_treynor = (average_annual_port_ret[[1]] - free)/model_beta
  cat("The portfolio treynor is", model_treynor)
}
