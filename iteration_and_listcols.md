Iteration and list columns
================
Wenzhao Wu
11/9/2020

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
set.seed(1)

knitr::opts_chunk$set(
  fig.width = 12,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

You can put everything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, nrow = 2, ncol = 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

## `for` loop

``` r
list_norms = 
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(40, 0, 5),
    c = rnorm(30, 10, .2),
    d = rnorm(20, -3, 1)
  )

is.list(list_norms)
```

    ## [1] TRUE

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

I can apply that function to each list element

``` r
mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.12  4.43

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.211

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.92  1.00

Let’s use a for loop.

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}
```

## Let’s try map\!

``` r
map(list_norms, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.12  4.43
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.211
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.92  1.00

What if I want a different function?

``` r
map(list_norms, median)
```

    ## $a
    ## [1] 2.621376
    ## 
    ## $b
    ## [1] 1.254102
    ## 
    ## $c
    ## [1] 9.981628
    ## 
    ## $d
    ## [1] -2.912783

``` r
map(list_norms, IQR)
```

    ## $a
    ## [1] 1.734464
    ## 
    ## $b
    ## [1] 5.860648
    ## 
    ## $c
    ## [1] 0.2667331
    ## 
    ## $d
    ## [1] 1.05479

``` r
output = map_dbl(list_norms, median, .id = "input")
```

``` r
output = map_dfr(list_norms, mean_and_sd, .id = "input")
```

## List columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norms
  )

listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 4.134965 4.111932 2.129222 3.210732 3.069396 1.337351 3.810840 1.087654
    ##  [9] 1.753247 3.998154 2.459127 2.783624 1.378063 1.549036 3.350910 2.825453
    ## [17] 2.408572 1.665973 1.902701 5.036104
    ## 
    ## $b
    ##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380 -6.26177962
    ##  [7] -5.04751876  3.75695597 -6.54176756  2.63770049 -2.66769787 -1.99188007
    ## [13] -3.94784725 -1.15070568  4.38592421  2.26866589 -1.16232074  4.35002762
    ## [19]  8.28001867 -0.03184464  2.35244727  1.39109325 -4.88951471 -4.63293071
    ## [25]  9.59885231  4.40638894  3.71040886  0.73786702  2.42694282  0.75928020
    ## [31]  0.20999377  1.11711156 -5.05232543 12.00611051  4.00980895 -1.25603979
    ## [37]  6.06444685 -3.13629043  8.55579254 -1.97186777
    ## 
    ## $c
    ##  [1]  9.535702 10.272824 10.226446  9.845137  9.717925  9.633094  9.946197
    ##  [8]  9.633214  9.837106 10.032714 10.171104  9.836007  9.975279 10.050990
    ## [15] 10.343785  9.808291  9.679138  9.630878 10.111147  9.987976 10.154417
    ## [22]  9.971832 10.078619 10.044844 10.004708  9.875407 10.252402  9.918845
    ## [29] 10.133353 10.032928
    ## 
    ## $d
    ##  [1] -1.218476 -2.288786 -3.337691 -3.009149 -3.125309 -5.090846 -1.302606
    ##  [8] -1.936119 -3.766617 -2.617992 -2.758104 -4.132759 -1.510093 -3.248247
    ## [15] -2.816416 -2.595129 -3.994124 -4.085429 -3.048543 -2.423914

``` r
listcol_df %>%
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.12  4.43
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.211
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.92  1.00

Add a new column

``` r
listcol_df = 
  listcol_df %>% 
  mutate(summary = map(samp, mean_and_sd),
         medians = map_dbl(samp, median))

listcol_df
```

    ## # A tibble: 4 x 4
    ##   name  samp         summary          medians
    ##   <chr> <named list> <named list>       <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 x 2]>    2.62
    ## 2 b     <dbl [40]>   <tibble [1 x 2]>    1.25
    ## 3 c     <dbl [30]>   <tibble [1 x 2]>    9.98
    ## 4 d     <dbl [20]>   <tibble [1 x 2]>   -2.91
