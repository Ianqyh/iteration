Iteration and Listcols
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)

knitr::opts_chunk$set(
  fig.width = 6,
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

## Define function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df =
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)

}
```

## Lists

``` r
l =
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE, FALSE),
    summary = summary(rnorm(1000), mean = 5, sd = 3)
  )

l[[3]]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.323523 -0.651741 -0.027881 -0.002738  0.653921  2.802510

``` r
l[["summary"]]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.323523 -0.651741 -0.027881 -0.002738  0.653921  2.802510

``` r
l$summary
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.323523 -0.651741 -0.027881 -0.002738  0.653921  2.802510

## List of normals

``` r
list_norms =
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.12  1.11

## for loop

Let’s use a for loop to iterate over my list of normals.

``` r
output = vector("list", length = 4)

for (i in 1:4){
  
  output[[i]] = mean_and_sd(list_norms[[i]])
  
}
```

Let’s use map instead …

``` r
output = map(list_norms, mean_and_sd)

output = map(list_norms, summary)

output = map_dbl(list_norms, median)
```

## LIST COLUMNS!!!!!

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )

listcol_df %>%
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1]  3.47787783  2.09894786  1.64561770  3.30361460  3.04707288  2.77559058
    ##  [7]  1.57573047  3.20611659 -0.12846592  3.09952222  3.83437882  0.21220153
    ## [13]  1.36046039  1.32721034  2.93805405  1.98545644  2.13557979  2.02872332
    ## [19]  3.17250316  2.59867334  2.18901904  3.92796460  1.52226055  3.48703563
    ## [25]  1.29260588  1.46455212  0.04590404  1.17254048  1.25169905  1.50277127
    ## [31]  0.70916980  3.02029852  3.18532123  0.83543547  2.16454546  2.67362036
    ## [37]  1.43249333  2.32667477  4.05928150  1.87085128  3.26641509  1.62453303
    ## [43]  3.39431560  3.53807326  0.39215555 -0.34245068  1.58516986  2.18147424
    ## [49]  1.51823887  2.78291837
    ## 
    ## $b
    ##  [1]  3.3213303  3.9138351  6.2344159  9.0218339  8.7728472  4.1979108
    ##  [7]  6.8517549  5.4032629  8.5691195  2.4164410 14.0311007  1.4546628
    ## [13]  2.5866043  4.3218826  4.1211980  7.8464735  3.1027074  3.8399797
    ## [19]  6.9280557  5.8762335  9.2585321  1.5741177  6.3460353  1.2353618
    ## [25]  5.0229284  6.9251281  8.3633049  8.3091139  4.7560204  6.5532458
    ## [31]  4.3719190  7.0132573  4.1100566  5.6793144  7.3970290  7.8784035
    ## [37]  6.8271429  7.6519533  4.1746937  7.3608502  3.0538659 -0.7107546
    ## [43]  6.9107889  4.6681295  6.3726897  6.5812342  6.8657481  7.4944468
    ## [49]  3.8176430  0.5421706
    ## 
    ## $c
    ##  [1] 21.34616 19.55115 18.03117 21.40238 19.25266 18.93476 18.89269 18.35585
    ##  [9] 20.46320 19.02071 18.43671 18.55864 19.28607 19.26013 20.03765 20.43558
    ## [17] 21.05495 22.45149 19.07012 22.08055 19.99618 20.40845 19.26922 21.13206
    ## [25] 19.40679 21.75093 17.71267 20.94392 19.27582 21.69737 18.60675 18.93022
    ## [33] 20.12141 17.98293 18.56573 18.08303 20.35573 20.02203 19.53821 17.85940
    ## [41] 20.20885 20.67214 19.59145 19.56810 18.82043 19.18323 19.50718 20.40672
    ## [49] 18.85228 17.08895
    ## 
    ## $d
    ##  [1] -11.16730 -12.24012 -12.10986 -11.20821 -13.18809 -11.36391 -12.10266
    ##  [8] -12.51690 -11.39123 -10.83105 -12.06986 -12.27355 -11.87703 -12.17543
    ## [15] -12.71549 -11.85513 -11.70228 -12.23287 -12.32574 -11.06117 -11.98099
    ## [22] -12.65138 -11.65072 -12.02684 -12.24379 -11.66123 -12.11147 -11.55200
    ## [29] -11.26934 -10.95912 -12.98097 -12.15211 -12.04984 -12.12604 -11.72491
    ## [36] -11.83357 -11.87793 -11.01337 -11.61063 -12.87908 -12.20107 -12.16697
    ## [43] -12.02446 -11.58285 -12.25700 -12.26232 -11.58118 -11.86718 -11.56553
    ## [50] -12.00891

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.12  1.11

``` r
listcol_df %>%
  mutate(summaries = map(norms, mean_and_sd))
```

    ## # A tibble: 4 × 3
    ##   name  norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-15 18:12:56 (7.605)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-15 18:13:02 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-15 18:13:05 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-10-31

Nest data within location

``` r
weather_nested = nest(weather_df, data = date:tmin)

unnest(weather_nested, data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>%
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

## Napoleon!!!

Function to get reviews / stars

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()

  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()

  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()

  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  return(reviews)

}


base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 it was                                                5.0 ou… "\n  mad good …
    ##  2 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  3 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  4 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  5 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  6 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  7 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  9 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ## 10 Classic Film                                          5.0 ou… "\n  Had to or…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 hehehehe                          5.0 out of 5 stars "\n  goodjobboys\n"     
    ##  2 Painful                           1.0 out of 5 stars "\n  I think I sneezed …
    ##  3 GRAND                             5.0 out of 5 stars "\n  GRAND\n"           
    ##  4 Hello, 90s                        5.0 out of 5 stars "\n  So nostalgic movie…
    ##  5 Cult Classic                      5.0 out of 5 stars "\n  Watched it with my…
    ##  6 Format was inaccurate             4.0 out of 5 stars "\n  There was an optio…
    ##  7 Good funny                        3.0 out of 5 stars "\n  Would recommend\n" 
    ##  8 Not available w/in 48 hour window 1.0 out of 5 stars "\n  I couldn't watch i…
    ##  9 Your mom went to college.         5.0 out of 5 stars "\n  Classic funny movi…
    ## 10 Very funny movie                  5.0 out of 5 stars "\n  I watch this movie…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  2 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  3 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  4 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  5 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  6 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ##  7 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ##  8 Hilarious                                   5.0 out of 5 stars "\n  Funny\n" 
    ##  9 Love it                                     5.0 out of 5 stars "\n  What of …
    ## 10 WORTH IT!                                   5.0 out of 5 stars "\n  It's the…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 Funny movie.                                  5.0 out of 5 stars "\n  Great …
    ##  2 Best movie ever!                              5.0 out of 5 stars "\n  Got th…
    ##  3 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  4 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  5 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  6 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ##  7 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ##  8 Perfect                                       5.0 out of 5 stars "\n  Exactl…
    ##  9 Love this movie!                              5.0 out of 5 stars "\n  Great …
    ## 10 Love it                                       5.0 out of 5 stars "\n  Love t…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 As described                      3.0 out of 5 stars "\n  Book is as describ…
    ##  2 GOSH!!!                           5.0 out of 5 stars "\n  Just watch the mov…
    ##  3 Watch it right now                5.0 out of 5 stars "\n  You need to watch …
    ##  4 At this point it’s an addiction   5.0 out of 5 stars "\n  I watch this movie…
    ##  5 💕                                5.0 out of 5 stars "\n  Hands down, one of…
    ##  6 Good dumb movie                   5.0 out of 5 stars "\n  I really wanted to…
    ##  7 funny                             5.0 out of 5 stars "\n  so funny and inven…
    ##  8 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n" 
    ##  9 Vote For Pedro!!                  5.0 out of 5 stars "\n  What is NOT to lik…
    ## 10 So Funny                          5.0 out of 5 stars "\n  This is such a goo…

``` r
napoleon_df =
  tibble(
    urls = urls
  )

napoleon_df %>%
  mutate(reviews = map(urls, get_page_reviews)) %>%
  select(reviews) %>%
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 it was                                                5.0 ou… "\n  mad good …
    ##  2 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  3 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  4 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  5 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  6 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  7 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  9 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ## 10 Classic Film                                          5.0 ou… "\n  Had to or…
    ## # … with 40 more rows
