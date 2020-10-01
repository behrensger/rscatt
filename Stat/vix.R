library(quantstrat)
library(blotter)
require(downloader)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
require(Quandl)
require(data.table)


# https://github.com/braverock/blotter
# https://github.com/braverock/quantstrat
# https://quantstrattrader.wordpress.com/
# http://kbroman.org/pkg_primer/pages/build.html



#
#library(curl)
#Sys.setenv(https_proxy = ie_get_proxy_for_url(target_url = "http://www.google.com"))
#Sys.setenv(http_proxy = ie_get_proxy_for_url(target_url = "https://www.google.com"))

#initial settings
Sys.setenv(TZ = "UTC")
currency('USD')
init_date <- "2007-12-31"
start_date <- "2008-01-01"
end_date <- "2009-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE


basic_symbols <- function() {
  symbols <- c("IWM", # iShares Russell 2000 Index ETF
               "QQQ", # PowerShares QQQ TRust, Series 1 ETF
               "SPY" # SPDR S&P 500 ETF Trust
               )
}

enhanced_symbols <- function() {
  symbols <- c(basic_symbols(),
               "TLT",
               # iShares Barclays 20+ Yr Treas. Bond ETF
               "XLB",
               # Materials Select Sector SPDR ETF
               "XLE",
               # Energy Select Sector SPDR ETF
               "XLF",
               # Financial Select Sector SPDR ETF
               "XLI",
               # Industrials Select Sector SPDR ETF
               "XLK",
               # Technology  Select Sector SPDR ETF
               "XLP",
               # Consumer Staples  Select Sector SPDR ETF
               "XLU",
               # Utilities  Select Sector SPDR ETF
               "XLV",
               # Health Care  Select Sector SPDR ETF
               "XLY" # Consumer Discretionary  Select Sector SPDR ETF
               )
}

global_symbols <- function() {
  symbols <- c(
    enhanced_symbols(),
    "EFA",
    # iShares EAFE
    "EPP",
    # iShares Pacific Ex Japan
    "EWA",
    # iShares Australia
    "EWC",
    # iShares Canada
    "EWG",
    # iShares Germany
    "EWH",
    # iShares Hong Kong
    "EWJ",
    # iShares Japan
    "EWS",
    # iShares Singapore
    "EWT",
    # iShares Taiwan
    "EWU",
    # iShares UK
    "EWY",
    # iShares South Korea
    "EWZ",
    # iShares Brazil
    "EZU",
    # iShares MSCI EMU ETF
    "IGE",
    # iShares North American Natural Resources
    "IYR",
    # iShares U.S. Real Estate
    "IYZ",
    # iShares U.S. Telecom
    "LQD",
    # iShares Investment Grade Corporate Bonds
    "SHY" # iShares 42372 year TBonds
  )
}

checkBlotterUpdate <- function(port.st = portfolio.st,
                               account.st = account.st,
                               verbose = TRUE) {
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(
    syms,
    FUN = function(x)
      eval(parse(
        text = paste("sum(p$symbols",
                     x,
                     "posPL.USD$Net.Trading.PL)",
                     sep = "$")
      ))
  ))
  
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  
  if (!isTRUE(all.equal(port.tot, port.sum.tot))) {
    ok <- FALSE
    if (verbose)
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  
  if (!isTRUE(all.equal(port.tot, endEq - initEq))) {
    ok <- FALSE
    if (verbose)
      print("portfolio P&L doesn't match account P&L")
  }
  
  if (sum(duplicated(index(p$summary)))) {
    ok <- FALSE
    if (verbose)
      print("duplicate timestamps in portfolio summary")
    
  }
  
  if (sum(duplicated(index(a$summary)))) {
    ok <- FALSE
    if (verbose)
      print("duplicate timestamps in account summary")
  }
  return(ok)
}

#Symbole holen
getSymbols(
  Symbols = basic_symbols(),
  src = 'yahoo',
  index.class = "POSIXct",
  from = start_date,
  to = end_date,
  adjust = adjustment
)

getSymbols(
  Symbols = c('DEXUSEU'), #EUR.USD
  src = 'FRED',
  index.class = "POSIXct",
  from = start_date,
  to = end_date,
  adjust = adjustment
)

#Anzeigen
chartSeries(DEXUSEU)
