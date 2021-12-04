# Purpose

The purpose of this README is to give the solutions to the Financial
Econometrics 2021 exam written on 04 December 2021. There are also
separate Texevier folders for each questions.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 395440 21.2     810358 43.3   638940 34.2
    ## Vcells 714707  5.5    8388608 64.0  1633464 12.5

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.3     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

``` r
# All programs and data used 

pacman::p_install_gh("Nicktz/fmxdat")
```

    ## Skipping install of 'fmxdat' from a github remote, the SHA1 (be8e46c2) has not changed since last install.
    ##   Use `force = TRUE` to force installation

    ## 
    ## The following packages were installed:
    ## fmxdat

``` r
library(tidyverse)
library(dplyr)
library(tbl2xts)
library(PerformanceAnalytics)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

``` r
SA_bonds <- read_rds("data/SA_Bonds.rds")
BE_Infl <- read_rds("data/BE_Infl.rds")
bonds_2y <- read_rds("data/bonds_2y.rds")
bonds_10y <- read_rds("data/bonds_10y.rds")
usdzar <- read_rds("data/usdzar.rds")
ZA_Infl <- read_rds("data/ZA_Infl.rds")
IV <- read_rds("data/IV.rds")
```

# Question 1

Question 1 requires an analysis of South Africa’s Yield Spreads and
local mid to longer dated yields bonds. There have been claims that the
bond yields are the highest they have been in decades. The plot below
confirms this. As seen in the graph, the lowest spread between the local
bond market was during the 2007/2008 financial crises, afterwhich it
dramatically increases in 2010. The dramatic increase was short lived,
with the yield spread increasing slowly between 2011 and 2019. In 2020,
the yield spread between the 10 Year and 2 year, as well as the yield
spread between the 10 year and 3 month local bonds increased
dramatically. The yield spread between the 2 year and 3 month local
bonds also experienced an increase, however, this increase reached
levels similar to those experienced in 2000/2001.

In increase in the yield spread can be explained by the uncertainty in
the markets during the still on-going COVID-19 pandemic. Investors
rushed to raise liquidity amid the uncertainty and panic, particularly
those regarded as having a hint of risk. South African government bonds
did not escape the effect of the pandemic, with significant sales of the
asset class by foreigners, causing yields to spike and prices to move
lower.

``` r
## SA_Bonds 
plot2 <- SA_bonds %>% mutate("10 Year and 3 Month"=SA_bonds$ZA_10Yr - SA_bonds$SA_3M, "2 Year and 3 Month" = SA_bonds$ZA_2Yr - SA_bonds$SA_3M, "10 Year and 2 Year"= SA_bonds$ZA_10Yr-SA_bonds$ZA_2Yr) %>% select(date, "10 Year and 3 Month", "2 Year and 3 Month", "10 Year and 2 Year") %>% gather(yield_spread, points, -date) %>% ggplot() + 
    geom_line(aes(x=date, y=points, color=yield_spread)) +
    theme_bw() +  
    labs(x = "Dates", y = "Spread", title = "Yeild Spread in Local Bond Market", subtitle = "Comparing Yeild Spread Between 3m, 2Y and 10Y bonds", caption = "Note:\nOwn Calculations") + guides(col=guide_legend("Yield Spread Between:"))

plot2
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

This effect felt are not isolated to the South African bond market, with
all emerging market local currency debt selling off in unison, as will
be shown in the graphs below. This graph follows a similar pattern ad
the local bonds yeild spread.

``` r
plot3 <- left_join(SA_bonds %>% select(date, ZA_10Yr), bonds_10y %>% group_by(bonds_10y$Name) %>% filter(Name=="US_10Yr") %>% ungroup() %>% arrange(date) %>% filter(date >= lubridate::ymd(19991206)) , by= "date") %>% select(-c("bonds_10y$Name", "Name")) %>% mutate( spread = ZA_10Yr - Bond_10Yr) %>% ggplot() + geom_line(aes(x=date, y=spread), color="purple") +
 theme_bw() +  
    labs(x = "Dates", y = "Spread", title = "Yield Spread Between US and SA 10 Year Bonds", subtitle = "", caption = "Note:\nOwn Calculations")

plot3
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

## USD/ZAR level

The spreads of the 10-year bond yields can be used to gauge currencies.
The general rule is that when the yield spread widens in favor of a
certain currency, that currency will appreciate against other
currencies.

Yield spreads are often seen as driven by monetary policy. Differences
in the exchange rate will drastically affect the expected yield. The
graph below shows that this is true for longer term bonds, however,
there is more volatility is shorter term bonds implying that the
exchange rate is not that good of a predictor and that monetary policy
changes will drastically affect the yield.

``` r
plot1 <- usdzar %>% ggplot()+ geom_line(aes(x=date, y=Price), color="blue") +
   
theme_bw() +  labs(x = "", y = "Prices (ZAR)", title = "USD/ZAR Exchange Rate", subtitle = "Data given in SA currency ", caption = "Note:\nOwn Calculations")
plot1
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

## Different Yeild Spreads

Comparing the 10 year and 2 year bond is important, so that investor can
determine whether a bond is fairly priced, cheap or expensive. It is a
sign of the risk premium for investing in one investment over another.

``` r
left_join(SA_bonds %>% select(date, ZA_10Yr), bonds_10y %>% tbl_xts(.,cols_to_xts= "Bond_10Yr", spread_by ="Name") %>% xts_tbl() %>% filter(date >= lubridate::ymd(19991206)) %>% select(date, AUS_10Yr, Canada_10Yr, EURO_10Yr,UK_10Yr) %>% arrange(date), by= "date") %>% mutate( ZA_AUS = ZA_10Yr - AUS_10Yr, ZA_Canada = ZA_10Yr -Canada_10Yr, ZA_EURO = ZA_10Yr - EURO_10Yr, ZA_UK = ZA_10Yr - UK_10Yr) %>% select(date, ZA_AUS,ZA_Canada, ZA_EURO, ZA_UK) %>% gather(country, spread, -date) %>%
 ggplot() + geom_line(aes(x=date, y=spread, color=country)) +
 theme_bw() +  
    labs(x = "Dates", y = "Spread", title = "Yeild Spread Between Different Countries and SA 10 Year Bonds", subtitle = "Countries include United Kingdom, Europe, Cananda and Australie", caption = "Note:\nOwn Calculations")
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
left_join(SA_bonds %>% select(date, ZA_2Yr), bonds_2y %>% tbl_xts(.,cols_to_xts= "Bond_2Yr", spread_by ="Name") %>% xts_tbl() %>% filter(date >= lubridate::ymd(19991206)) %>% select(date, AUS_2yr, Canada_2yr, EURO_2yr,UK_2yr, US_2yr) %>% arrange(date), by= "date") %>% mutate( ZA_AUS = ZA_2Yr - AUS_2yr, ZA_Canada = ZA_2Yr -Canada_2yr, ZA_EURO = ZA_2Yr - EURO_2yr, ZA_UK = ZA_2Yr - UK_2yr, ZA_US = ZA_2Yr - US_2yr) %>% select(date, ZA_AUS,ZA_Canada, ZA_EURO, ZA_UK, ZA_US) %>% gather(country, spread, -date) %>%
 ggplot() + geom_line(aes(x=date, y=spread, color=country)) +
 theme_bw() +  
    labs(x = "Dates", y = "Spread", title = "Yeild Spread Between Different Countries and SA 2 Year Bonds", subtitle = "Countries include United Kingdom, Europe, Cananda, Australia, United States", caption = "Note:\nOwn Calculations")
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png) \#\#
Inflation In principle, the yield spread contains information on
expectations of future inflation that can be extracted as an estimate of
them. Studies of US and other markets have demonstrated that the yield
curve does contain information on expected and actual future levels of
gross domestic product (GDP) growth, and can be a lead indicator of
business cycle downturns.

Although many studies indicate the inflation-predicting power of the
yield curve in developed countries’ markets, little is known of the
relationship in emerging economies. The graph below mostly supports the
notion that the yield spread contains valuable information about
expected inflation, where price refers to the inflation rate. However,
it is important to notice that inflation has much higher volatility then
yield spread. The graph below shows that the higher the current rate of
inflation, the higher the yields will rise across the yield curve, as
investors will demand this higher yield to compensate for inflation risk

``` r
data <- left_join(SA_bonds %>% select(date, ZA_10Yr), bonds_10y %>% group_by(bonds_10y$Name) %>% filter(Name=="US_10Yr") %>% ungroup() %>% arrange(date) %>% filter(date >= lubridate::ymd(19991206)) , by= "date") %>% select(-c("bonds_10y$Name", "Name")) %>% mutate( spread = ZA_10Yr - Bond_10Yr) %>% select(date, spread)

left_join(ZA_Infl %>%  filter(date >= lubridate::ymd(19991206)) %>% select(-Name) , data, by = "date") %>% na.omit() %>% gather(Data, Values, -date) %>% ggplot() + geom_line(aes(x=date, y=Values, color=Data)) +
 theme_bw() +  
    labs(x = "Dates", y = "", title = "Comparing Inflation and Yield Spread", subtitle = "Yeild Spread is between US and ZAR 10 year bond", caption = "Note:\nOwn Calculations")
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

# Question 2
