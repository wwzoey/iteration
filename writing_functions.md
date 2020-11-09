Writing Functions
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
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
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

set.seed(1)
```

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.767120446  0.109493425 -0.993470503  1.637039863  0.267334741
    ##  [6] -0.977065475  0.438222871  0.709719461  0.533829741 -0.419692475
    ## [11]  1.546684110  0.332624325 -0.761479160 -2.485776741  1.128069748
    ## [16] -0.137851865 -0.106748415  0.932105430  0.799422547  0.553437533
    ## [21]  0.905205442  0.757128408 -0.008541293 -2.241925304  0.581490604
    ## [26] -0.149966223 -0.257816586 -1.680744021 -0.606639531  0.363029790

I want a function to compute z-scores

``` r
z_scores = function(x) {
        
  if (!is.numeric(x)) {
     stop("Argument x should be numeric")      
  }        
  
  if (length(x) < 3) {
     stop("Input must have at least 3 numbers")
  }     
  z = (x - mean(x)) / sd(x)
  
  return(z)
              
}

z_scores(x_vec)
```

    ##  [1] -0.767120446  0.109493425 -0.993470503  1.637039863  0.267334741
    ##  [6] -0.977065475  0.438222871  0.709719461  0.533829741 -0.419692475
    ## [11]  1.546684110  0.332624325 -0.761479160 -2.485776741  1.128069748
    ## [16] -0.137851865 -0.106748415  0.932105430  0.799422547  0.553437533
    ## [21]  0.905205442  0.757128408 -0.008541293 -2.241925304  0.581490604
    ## [26] -0.149966223 -0.257816586 -1.680744021 -0.606639531  0.363029790

Try my function on some other things. These should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least 3 numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Argument x should be numeric

``` r
z_scores(iris)
```

    ## Error in z_scores(iris): Argument x should be numeric

``` r
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ## Error in z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE)): Argument x should be numeric

## Multiple outputs

``` r
mean_and_sd = function(x) {
        
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")      
  }        
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }   
        
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
              
}
```

Check that the function works.

``` r
x_vec = rnorm(100, mean = 0, sd = 1)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0719 0.899

## Multiple inputs

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 3, sd = 2)
  )

sim_data %>%
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.14  2.02

``` r
sim_mean_sd = function(samp_size, mu, sigma) {
        
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
    )

  sim_data %>%
    summarize(
      mean = mean(x),
      sd = sd(x)
    )      
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.04  2.97

## Review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews…

Let’s turn that code into a function

``` r
read_page_reviews = function(url) {

  html = read_html(url)

  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()

  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
    )
  
  reviews
}
```

Let’s try my function

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 "Movie is still silly fun....amazo~     1 "We are getting really frustrated ~
    ##  2 "Brilliant and awkwardly funny."        5 "I've watched this movie repeatedl~
    ##  3 "Great purchase price for great mo~     5 "Great movie and real good digital~
    ##  4 "Movie for memories"                    5 "I've been looking for this movie ~
    ##  5 "Love!"                                 5 "Love this movie. Great quality"   
    ##  6 "Hilarious!"                            5 "Such a funny movie, definitely br~
    ##  7 "napoleon dynamite"                     5 "cool movie"                       
    ##  8 "Top 5"                                 5 "Best MOVIE ever! Funny one liners~
    ##  9 "\U0001f44d"                            5 "Exactly as described and came on ~
    ## 10 "A top favorite movie !!"               5 "Love this movie, needed to add it~

Let’s read a few pages of reviews

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5]))
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 0.8269961
