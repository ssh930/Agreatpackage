% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Annualized_Return.R
\name{ann_ret}
\alias{ann_ret}
\title{Annualized Return of Portfolio}
\usage{
ann_ret(tickers, wts, from, to)
}
\arguments{
\item{tickers}{This should be a string or a vector of strings. It is the tickers of stocks you want to look at.}

\item{wts}{This should be a double or a vector of doubles. It is the weights for each according ticker in your portfolio.}

\item{from}{This should be a string in form of 'yyyy-mm-dd'. It is the start of the time interval of your portfolio.}

\item{to}{This should be a string in form of 'yyyy-mm-dd'. It is the end of the time interval of your portfolio.}
}
\description{
Annualized Return of Portfolio
}
\examples{
ann_ret(c('JNJ' , 'WFC' , 'KO') , c(0.4 , 0.4 , -0.2) , '2020-11-11' , '2020-12-11')
}
