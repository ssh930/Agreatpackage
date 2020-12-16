#'Cumulative Profit Curve of Portfolio
#'
#'@param tickers This should be a string or a vector of strings. It is the tickers of stocks you want to look at.
#'
#'@param wts This should be a double or a vector of doubles. It is the weights for each according ticker in your portfolio.
#'
#'@param from This should be a string in form of 'yyyy-mm-dd'. It is the start of the time interval of your portfolio.
#'
#'@param to This should be a string in form of 'yyyy-mm-dd'. It is the end of the time interval of your portfolio.
#'
#'@examples
#'cum_ret(c('JNJ' , 'WFC' , 'KO') , c(0.4 , 0.4 , -0.2) , '2020-11-11' , '2020-12-11')
#'@importFrom tidyquant tq_get
#'@importFrom tidyquant tq_transmute
#'@importFrom tidyquant tq_portfolio
#'@importFrom dplyr tibble
#'@importFrom dplyr left_join
#'@importFrom dplyr mutate
#'@importFrom dplyr group_by
#'@importFrom magrittr "%>%"
#'@importFrom stats lm
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 theme_classic
#'@importFrom ggplot2 scale_y_continuous
#'@export
cum_ret <- function(tickers , wts , from , to) {
  price_data <- tidyquant::tq_get(tickers , from = from , to = to , get = 'stock.prices')

  ret_data <- price_data %>%
    group_by(symbol) %>%
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

  port_cumulative_ret <- port_ret %>%
    dplyr::mutate(cr = cumprod(1 + port_ret))

  port_cumulative_ret %>%
    ggplot2::ggplot(aes(x = date, y = cr)) +
    geom_line() +
    labs(x = 'Date',
         y = 'Cumulative Returns',
         title = 'Portfolio Cumulative Returns') +
    theme_classic() +
    scale_y_continuous(breaks = seq(1,2,0.1))
}
