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
rm(list = ls()) # Clean your environment:
gc() 
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 395485 21.2     810487 43.3   638940 34.2
    ## Vcells 714553  5.5    8388608 64.0  1633459 12.5

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
library(dplyr)
library(rportfolios)
```

    ## Loading required package: truncdist

    ## Loading required package: stats4

    ## Loading required package: evd

``` r
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
library(devtools)
```

    ## Loading required package: usethis

``` r
library(rmsfuns)
pacman::p_install_gh("Nicktz/fmxdat")
```

    ## Skipping install of 'fmxdat' from a github remote, the SHA1 (be8e46c2) has not changed since last install.
    ##   Use `force = TRUE` to force installation

    ## 
    ## The following packages were installed:
    ## fmxdat

``` r
pacman::p_load(fEcofin)
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

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
    ##  $ sdev    : num [1:92] 2.85 1.64 1.31 1.25 1.14 ...
    ##  $ rotation: num [1:92, 1:92] 0.2572 0.01762 0.00282 0.21252 0.14249 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:92] "ABG" "ACL" "AEG" "AGL" ...
    ##   .. ..$ : chr [1:92] "PC1" "PC2" "PC3" "PC4" ...
    ##  $ center  : Named num [1:92] 0.00000431 -0.00000771 0.00004076 0.00001858 0.00000261 ...
    ##   ..- attr(*, "names")= chr [1:92] "ABG" "ACL" "AEG" "AGL" ...
    ##  $ scale   : Named num [1:92] 0.000281 0.000822 0.000944 0.002478 0.000505 ...
    ##   ..- attr(*, "names")= chr [1:92] "ABG" "ACL" "AEG" "AGL" ...
    ##  $ x       : num [1:3458, 1:92] 2.58 -1.43 -1.44 -2.15 2.16 ...
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
    ##                            PC1    PC2     PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.84888 1.6363 1.31479 1.25174 1.13845 1.13437 1.12620
    ## Proportion of Variance 0.08822 0.0291 0.01879 0.01703 0.01409 0.01399 0.01379
    ## Cumulative Proportion  0.08822 0.1173 0.13611 0.15314 0.16723 0.18122 0.19500
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.11672 1.10504 1.09840 1.09580 1.09402 1.09114 1.08666
    ## Proportion of Variance 0.01356 0.01327 0.01311 0.01305 0.01301 0.01294 0.01284
    ## Cumulative Proportion  0.20856 0.22183 0.23495 0.24800 0.26101 0.27395 0.28678
    ##                           PC15    PC16   PC17   PC18    PC19    PC20    PC21
    ## Standard deviation     1.08431 1.08067 1.0724 1.0682 1.06504 1.05725 1.05608
    ## Proportion of Variance 0.01278 0.01269 0.0125 0.0124 0.01233 0.01215 0.01212
    ## Cumulative Proportion  0.29956 0.31226 0.3248 0.3372 0.34949 0.36164 0.37376
    ##                           PC22    PC23    PC24    PC25   PC26    PC27    PC28
    ## Standard deviation     1.04928 1.04745 1.04418 1.04096 1.0376 1.03422 1.03171
    ## Proportion of Variance 0.01197 0.01193 0.01185 0.01178 0.0117 0.01163 0.01157
    ## Cumulative Proportion  0.38573 0.39766 0.40951 0.42128 0.4330 0.44461 0.45618
    ##                          PC29    PC30   PC31    PC32    PC33    PC34    PC35
    ## Standard deviation     1.0285 1.02327 1.0194 1.01767 1.01475 1.00860 1.00481
    ## Proportion of Variance 0.0115 0.01138 0.0113 0.01126 0.01119 0.01106 0.01097
    ## Cumulative Proportion  0.4677 0.47906 0.4904 0.50162 0.51281 0.52387 0.53484
    ##                           PC36    PC37    PC38    PC39    PC40    PC41    PC42
    ## Standard deviation     1.00372 1.00103 0.99944 0.99561 0.99028 0.98800 0.98643
    ## Proportion of Variance 0.01095 0.01089 0.01086 0.01077 0.01066 0.01061 0.01058
    ## Cumulative Proportion  0.54579 0.55668 0.56754 0.57832 0.58897 0.59959 0.61016
    ##                           PC43    PC44    PC45    PC46    PC47    PC48   PC49
    ## Standard deviation     0.98439 0.98184 0.97630 0.97052 0.96917 0.96355 0.9592
    ## Proportion of Variance 0.01053 0.01048 0.01036 0.01024 0.01021 0.01009 0.0100
    ## Cumulative Proportion  0.62069 0.63117 0.64153 0.65177 0.66198 0.67207 0.6821
    ##                           PC50    PC51    PC52   PC53    PC54    PC55    PC56
    ## Standard deviation     0.95791 0.95679 0.95099 0.9494 0.94792 0.94308 0.93439
    ## Proportion of Variance 0.00997 0.00995 0.00983 0.0098 0.00977 0.00967 0.00949
    ## Cumulative Proportion  0.69205 0.70200 0.71183 0.7216 0.73139 0.74106 0.75055
    ##                           PC57   PC58    PC59    PC60    PC61    PC62    PC63
    ## Standard deviation     0.93273 0.9299 0.92765 0.92604 0.92220 0.92144 0.91802
    ## Proportion of Variance 0.00946 0.0094 0.00935 0.00932 0.00924 0.00923 0.00916
    ## Cumulative Proportion  0.76001 0.7694 0.77876 0.78808 0.79732 0.80655 0.81571
    ##                           PC64    PC65   PC66    PC67    PC68    PC69    PC70
    ## Standard deviation     0.91278 0.90801 0.9048 0.90106 0.89175 0.89031 0.88326
    ## Proportion of Variance 0.00906 0.00896 0.0089 0.00883 0.00864 0.00862 0.00848
    ## Cumulative Proportion  0.82477 0.83373 0.8426 0.85146 0.86010 0.86871 0.87719
    ##                           PC71    PC72    PC73   PC74    PC75    PC76    PC77
    ## Standard deviation     0.88068 0.87324 0.86384 0.8579 0.84617 0.83537 0.82601
    ## Proportion of Variance 0.00843 0.00829 0.00811 0.0080 0.00778 0.00759 0.00742
    ## Cumulative Proportion  0.88562 0.89391 0.90202 0.9100 0.91781 0.92539 0.93281
    ##                           PC78    PC79   PC80    PC81    PC82    PC83    PC84
    ## Standard deviation     0.81855 0.79978 0.7911 0.76050 0.75798 0.68914 0.67367
    ## Proportion of Variance 0.00728 0.00695 0.0068 0.00629 0.00624 0.00516 0.00493
    ## Cumulative Proportion  0.94009 0.94704 0.9538 0.96013 0.96638 0.97154 0.97647
    ##                           PC85    PC86    PC87    PC88   PC89    PC90    PC91
    ## Standard deviation     0.66147 0.62356 0.58879 0.51538 0.4986 0.48382 0.40310
    ## Proportion of Variance 0.00476 0.00423 0.00377 0.00289 0.0027 0.00254 0.00177
    ## Cumulative Proportion  0.98123 0.98546 0.98922 0.99211 0.9948 0.99736 0.99912
    ##                           PC92
    ## Standard deviation     0.28385
    ## Proportion of Variance 0.00088
    ## Cumulative Proportion  1.00000

``` r
plot(pca, type = "l")
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

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

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

With the 92 PCA, the information that we gain is not very useful.
