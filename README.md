# Purpose

The purpose of this README is to give the solutions to the Financial
Econometrics 2021 exam written on 04 December 2021. There are also
separate Texevier folders for each questions.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 395661 21.2     810990 43.4   638940 34.2
    ## Vcells 716498  5.5    8388608 64.0  1633464 12.5

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
pacman::p_load(tbl2xts, PerformanceAnalytics)
library(tidyverse)
library(dplyr)
library(rportfolios)
```

    ## Loading required package: truncdist

    ## Loading required package: stats4

    ## Loading required package: evd

``` r
library(PerformanceAnalytics)
library(rmsfuns)
library(devtools)
```

    ## Loading required package: usethis

``` r
if (!require(FactoMineR)) install.packages("FactoMineR")
```

    ## Loading required package: FactoMineR

``` r
if (!require(factoextra)) install.packages("factoextra")
```

    ## Loading required package: factoextra

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(factoextra)
library(FactoMineR)
pacman::p_load("tidyr", "tbl2xts","devtools","lubridate", "readr", "PerformanceAnalytics", "ggplot2", "dplyr")
SA_bonds <- read_rds("data/SA_Bonds.rds")
BE_Infl <- read_rds("data/BE_Infl.rds")
bonds_2y <- read_rds("data/bonds_2y.rds")
bonds_10y <- read_rds("data/bonds_10y.rds")
usdzar <- read_rds("data/usdzar.rds")
ZA_Infl <- read_rds("data/ZA_Infl.rds")
IV <- read_rds("data/IV.rds")
T40 <- read_rds("data/T40.rds")
RebDays <- read_rds("data/Rebalance_days.rds")
msci <- read_rds("data/msci.rds")
bonds <- read_rds("data/bonds_10y.rds")
comms <- read_rds("data/comms.rds")
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

# Question 3

With the code, I can confirm that there are 92 PCA. This number is a
large, however, there are 92 unique tickets in the Top40.This is because
every year, some stocks get dropped form the Top40, while others get
added. Each of the 92 components explains a percentage of the total
variation in the T40 returns. That is, PC1 explains 8 % of the total
variance, which is the most out of the 92 PCA’s. PC50 to PC92 all
explain less than 1 percent of the total variance. The table below plots
the individuals components importance.

``` r
#Calculating returns
T40 <- read_rds("data/T40.rds")

T40[sapply(T40, is.infinite)] <- NA
T40[T40 == 0] <- NA
    
T40 <- read_rds("data/T40.rds") %>% na.locf(.,na.rm=T, 5) %>%
    select(date, Tickers, Return, J200) %>%
    mutate(Return = Return*J200) %>%
    select(date, Tickers, Return) %>% 
    group_by(Tickers) %>%
    mutate(Tickers = gsub(" SJ Equity", "", Tickers))

return_mat <- T40 %>% spread(Tickers,Return) 
colSums(is.na(T40))
```

    ##    date Tickers  Return 
    ##       0       0       0

``` r
impute_missing_returns <- function(return_mat, impute_returns_method = "NONE", Seed = 1234){
  # Make sure we have a date column called date:
  if( !"date" %in% colnames(return_mat) ) stop("No 'date' column provided in return_mat. Try again please.")

  # Note my use of 'any' below...
  # Also note that I 'return' return_mat - which stops the function and returns return_mat. 
  if( impute_returns_method %in% c("NONE", "None", "none") ) {
    if( any(is.na(return_mat)) ) warning("There are missing values in the return matrix.. Consider maybe using impute_returns_method = 'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
    return(return_mat)
  }

  
  if( impute_returns_method  == "Average") {

    return_mat <-
      return_mat %>% gather(Stocks, Returns, -date) %>%
      group_by(date) %>%
      mutate(Avg = mean(Returns, na.rm=T)) %>%
      mutate(Avg = coalesce(Avg, 0)) %>% # date with no returns - set avg to zero
      ungroup() %>%
      mutate(Returns = coalesce(Returns, Avg)) %>% select(-Avg) %>% spread(Stocks, Returns)

    # That is just so much easier when tidy right? See how I gathered and spread again to give back a wide df?
    
  } else

    if( impute_returns_method  == "Drawn_Distribution_Own") {

      set.seed(Seed)
      N <- nrow(return_mat)
      return_mat <-

        left_join(return_mat %>% gather(Stocks, Returns, -date),
                  return_mat %>% gather(Stocks, Returns, -date) %>% group_by(Stocks) %>%
                    do(Dens = density(.$Returns, na.rm=T)) %>%
                    ungroup() %>% group_by(Stocks) %>% # done to avoid warning.
                    do(Random_Draws = sample(.$Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y)),
                  by = "Stocks"
        ) %>%  group_by(Stocks) %>% mutate(Row = row_number()) %>% mutate(Returns = coalesce(Returns, Random_Draws[[1]][Row])) %>%
        select(-Random_Draws, -Row) %>% ungroup() %>% spread(Stocks, Returns)

    } else

      if( impute_returns_method  == "Drawn_Distribution_Collective") {

        set.seed(Seed)
        NAll <- nrow(return_mat %>% gather(Stocks, Returns, -date))

        return_mat <-
          bind_cols(
          return_mat %>% gather(Stocks, Returns, -date),
          return_mat %>% gather(Stocks, Returns, -date) %>%
            do(Dens = density(.$Returns, na.rm=T)) %>%
            do(Random_Draws = sample(.$Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y)) %>% unnest(Random_Draws)
          ) %>%
          mutate(Returns = coalesce(Returns, Random_Draws)) %>% select(-Random_Draws) %>% spread(Stocks, Returns)

      } else

        if( impute_returns_method  == "Zero") {
        warning("This is probably not the best idea but who am I to judge....")
          return_mat[is.na(return_mat)] <- 0

        } else
          stop("Please provide a valid impute_returns_method method. Options include:\n'Average', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")
}


options(scipen = 999) # Stop the scientific notation of

return_mat <- impute_missing_returns(return_mat, impute_returns_method = "Drawn_Distribution_Collective", Seed = as.numeric(format( Sys.time(), "%Y%d%H%M")))

return_mat_Nodate <- data.matrix(return_mat[, -1])

# Simple Sample covariance and mean:
Sigma <- RiskPortfolios::covEstimation(return_mat_Nodate)
Mu <- RiskPortfolios::meanEstimation(return_mat_Nodate)

#PCA calculations
pca <- prcomp(return_mat_Nodate,center=TRUE, scale.=TRUE)
d <- str(pca)
```

    ## List of 5
    ##  $ sdev    : num [1:92] 2.82 1.6 1.31 1.24 1.14 ...
    ##  $ rotation: num [1:92, 1:92] 0.2596 0.022 0.0111 0.2127 0.1217 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:92] "ABG" "ACL" "AEG" "AGL" ...
    ##   .. ..$ : chr [1:92] "PC1" "PC2" "PC3" "PC4" ...
    ##  $ center  : Named num [1:92] 0.00000431 -0.00000612 0.00001849 0.00001858 0.00000792 ...
    ##   ..- attr(*, "names")= chr [1:92] "ABG" "ACL" "AEG" "AGL" ...
    ##  $ scale   : Named num [1:92] 0.000281 0.000843 0.000831 0.002478 0.00057 ...
    ##   ..- attr(*, "names")= chr [1:92] "ABG" "ACL" "AEG" "AGL" ...
    ##  $ x       : num [1:3458, 1:92] 2.19 -2.03 -2.22 -3.19 2.47 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr [1:92] "PC1" "PC2" "PC3" "PC4" ...
    ##  - attr(*, "class")= chr "prcomp"

``` r
c <- pca$sdev #extracting standard deviation.
eigenvalues <- pca$sdev^2
b<- pca$rotation
a <- summary(pca)
a
```

    ## Importance of components:
    ##                            PC1     PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.81843 1.60440 1.30697 1.24477 1.14201 1.12508 1.12183
    ## Proportion of Variance 0.08634 0.02798 0.01857 0.01684 0.01418 0.01376 0.01368
    ## Cumulative Proportion  0.08634 0.11432 0.13289 0.14973 0.16391 0.17767 0.19135
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.11687 1.10520 1.10165 1.09514 1.08829 1.08222 1.07915
    ## Proportion of Variance 0.01356 0.01328 0.01319 0.01304 0.01287 0.01273 0.01266
    ## Cumulative Proportion  0.20490 0.21818 0.23137 0.24441 0.25728 0.27001 0.28267
    ##                           PC15    PC16    PC17    PC18    PC19    PC20    PC21
    ## Standard deviation     1.07320 1.06893 1.06400 1.06286 1.06034 1.05628 1.05017
    ## Proportion of Variance 0.01252 0.01242 0.01231 0.01228 0.01222 0.01213 0.01199
    ## Cumulative Proportion  0.29519 0.30761 0.31992 0.33219 0.34442 0.35654 0.36853
    ##                          PC22    PC23   PC24    PC25    PC26    PC27    PC28
    ## Standard deviation     1.0462 1.04359 1.0419 1.03851 1.03694 1.03231 1.02883
    ## Proportion of Variance 0.0119 0.01184 0.0118 0.01172 0.01169 0.01158 0.01151
    ## Cumulative Proportion  0.3804 0.39227 0.4041 0.41579 0.42748 0.43906 0.45056
    ##                           PC29    PC30    PC31    PC32    PC33   PC34    PC35
    ## Standard deviation     1.02652 1.02386 1.01897 1.01733 1.01364 1.0107 1.00996
    ## Proportion of Variance 0.01145 0.01139 0.01129 0.01125 0.01117 0.0111 0.01109
    ## Cumulative Proportion  0.46202 0.47341 0.48470 0.49595 0.50712 0.5182 0.52931
    ##                          PC36    PC37    PC38    PC39    PC40    PC41    PC42
    ## Standard deviation     1.0061 1.00339 0.99982 0.99320 0.99176 0.98835 0.98632
    ## Proportion of Variance 0.0110 0.01094 0.01087 0.01072 0.01069 0.01062 0.01057
    ## Cumulative Proportion  0.5403 0.55125 0.56212 0.57284 0.58353 0.59415 0.60472
    ##                           PC43    PC44    PC45    PC46    PC47    PC48    PC49
    ## Standard deviation     0.98393 0.98158 0.97866 0.97498 0.97142 0.96735 0.96519
    ## Proportion of Variance 0.01052 0.01047 0.01041 0.01033 0.01026 0.01017 0.01013
    ## Cumulative Proportion  0.61525 0.62572 0.63613 0.64646 0.65672 0.66689 0.67702
    ##                           PC50    PC51    PC52    PC53    PC54    PC55    PC56
    ## Standard deviation     0.96101 0.95852 0.95580 0.95277 0.95007 0.94613 0.94360
    ## Proportion of Variance 0.01004 0.00999 0.00993 0.00987 0.00981 0.00973 0.00968
    ## Cumulative Proportion  0.68705 0.69704 0.70697 0.71684 0.72665 0.73638 0.74606
    ##                           PC57   PC58    PC59    PC60    PC61    PC62    PC63
    ## Standard deviation     0.94076 0.9399 0.93629 0.93381 0.93037 0.92351 0.92260
    ## Proportion of Variance 0.00962 0.0096 0.00953 0.00948 0.00941 0.00927 0.00925
    ## Cumulative Proportion  0.75568 0.7653 0.77481 0.78429 0.79369 0.80296 0.81222
    ##                           PC64    PC65    PC66    PC67    PC68    PC69    PC70
    ## Standard deviation     0.91367 0.90905 0.90547 0.90051 0.89590 0.89118 0.88563
    ## Proportion of Variance 0.00907 0.00898 0.00891 0.00881 0.00872 0.00863 0.00853
    ## Cumulative Proportion  0.82129 0.83027 0.83918 0.84800 0.85672 0.86536 0.87388
    ##                           PC71    PC72    PC73    PC74    PC75    PC76    PC77
    ## Standard deviation     0.87943 0.87255 0.86961 0.85892 0.84668 0.84475 0.83494
    ## Proportion of Variance 0.00841 0.00828 0.00822 0.00802 0.00779 0.00776 0.00758
    ## Cumulative Proportion  0.88229 0.89056 0.89878 0.90680 0.91459 0.92235 0.92993
    ##                           PC78    PC79    PC80    PC81   PC82   PC83   PC84
    ## Standard deviation     0.83180 0.81416 0.80399 0.76539 0.7614 0.7178 0.6982
    ## Proportion of Variance 0.00752 0.00721 0.00703 0.00637 0.0063 0.0056 0.0053
    ## Cumulative Proportion  0.93745 0.94465 0.95168 0.95805 0.9644 0.9699 0.9752
    ##                           PC85    PC86   PC87    PC88    PC89    PC90    PC91
    ## Standard deviation     0.67305 0.66180 0.6213 0.51713 0.50158 0.48643 0.40432
    ## Proportion of Variance 0.00492 0.00476 0.0042 0.00291 0.00273 0.00257 0.00178
    ## Cumulative Proportion  0.98017 0.98493 0.9891 0.99204 0.99477 0.99734 0.99912
    ##                           PC92
    ## Standard deviation     0.28473
    ## Proportion of Variance 0.00088
    ## Cumulative Proportion  1.00000

``` r
fviz_screeplot(pca, ncp = 10)
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

As can be seen in the plot below, there is not one variable that makes a
significant contribution to the variance. Therefore, you will need to
consider all variables of the Top40 to understand how the variance is
affected. This graph is not very informative as we don’t know which
graph belongs to which Ticker. However, adding in rownames makes the
plot very messy.

``` r
install_github("vqv/ggbiplot")
```

    ## Skipping install of 'ggbiplot' from a github remote, the SHA1 (7325e880) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(ggbiplot)
```

    ## Loading required package: plyr

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    ## Loading required package: scales

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

    ## Loading required package: grid

``` r
ggbiplot(pca)
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

With the 92 PCA, the information that we gain is not very useful.

## ROlling Return

The graph below gives the cumulative returns for all the unique stocks
in the Top40, with different starting dates. Sinces we are analyzing 92
different stocks, the graph is still not clear as to which stocks
contribute the most to the top40 portfolio.

``` r
gg <-  read_rds("data/T40.rds")%>%
    select(date, Tickers, Return, J200) %>%
    mutate(Return = Return*J200) %>%
    select(date, Tickers, Return) %>% 
    arrange(date) %>%
    group_by(Tickers) %>%
    mutate(Tickers = gsub(" SJ Equity", "", Tickers)) %>% 
        mutate(Rets = coalesce(Return, 0)) %>%  
        mutate(CP = cumprod(1 + Rets)) %>% 
        ungroup() %>% ggplot() + 
geom_line(aes(date, CP, color = Tickers)) + 
labs(title = "Cumulative Returns of various Indices", 
    subtitle = "", caption = "Note:\nDistortions emerge as starting dates differ.")
# Level plot
gg
```

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)
