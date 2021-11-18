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

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.11280 -0.71652 -0.04539 -0.02804  0.64654  3.25611

``` r
l[["summary"]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.11280 -0.71652 -0.04539 -0.02804  0.64654  3.25611

``` r
l$summary
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.11280 -0.71652 -0.04539 -0.02804  0.64654  3.25611

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
    ## 1  2.09 0.921

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
    ##  [1] 1.1852030 2.2660430 3.3587189 0.1220380 1.8761734 1.6211507 3.4212861
    ##  [8] 1.3440008 0.8215310 1.5613426 3.3044172 2.3972736 2.1074806 1.9859222
    ## [15] 1.5293159 1.8540953 1.3270046 2.2228399 2.1788768 0.4591707 2.4137368
    ## [22] 2.9972648 2.1466699 2.7446573 1.3331487 2.0378465 2.0486648 2.4675600
    ## [29] 2.7632317 1.5989876 1.9812088 1.5146265 2.3045770 3.2081698 2.9029569
    ## [36] 1.9945974 3.9488084 1.6942873 1.7002782 1.0085075 3.3250953 1.6921481
    ## [43] 3.3014524 4.3958316 0.1173308 3.1270924 1.1267324 1.7073464 2.5553206
    ## [50] 1.2145929
    ## 
    ## $b
    ##  [1]  7.9753523  2.0294605  4.5069273 -0.6460872  4.0258227  4.3105315
    ##  [7] 10.4920819  5.8575490 -1.6660392  2.8006104  7.5607398  6.4100446
    ## [13]  7.4981130  3.9437269  4.0184743  4.4859185  8.9990283  6.1123405
    ## [19]  3.4649448  9.4005260  3.1133071  1.9426639  8.3098975  2.0225598
    ## [25]  1.8279037  0.7143606  3.1764971  2.0147845  8.0550732  0.9262363
    ## [31]  3.1730368  6.3106717  7.3824567  3.7582430  7.8535254  1.5249971
    ## [37]  8.8704050  3.3244768  6.8049227  9.3421348  4.0309896  7.4720839
    ## [43]  4.6171178  7.9681659  6.4596653  7.5537644  2.7557683  4.4823448
    ## [49]  8.1276638  4.1284009
    ## 
    ## $c
    ##  [1] 20.38376 17.87705 18.99976 20.38501 18.88123 19.59413 20.06095 20.77666
    ##  [9] 20.04453 20.54948 19.96011 21.67164 20.45196 20.65111 20.83397 19.43294
    ## [17] 19.36026 18.38081 21.38620 19.38074 20.65323 21.38007 20.82532 18.41521
    ## [25] 19.81499 20.08493 18.40748 19.41899 21.23676 20.10358 20.78507 18.71688
    ## [33] 20.55776 20.02243 19.95846 20.20619 20.31964 21.07155 18.77637 19.72349
    ## [41] 18.77973 21.11348 18.59199 17.40377 19.11990 17.29447 19.32063 20.89118
    ## [49] 20.30895 21.05653
    ## 
    ## $d
    ##  [1] -12.38493 -11.73198 -12.18145 -12.00085 -12.25298 -11.81612 -10.31377
    ##  [8] -12.07843 -11.69482 -12.85299 -10.85287 -12.42854 -12.45839 -11.44673
    ## [15] -11.66956 -12.57903 -12.09273 -11.95404 -12.65772 -12.02105 -12.26362
    ## [22] -12.41379 -12.23697 -11.81490 -11.23961 -11.86417 -11.51199 -12.99082
    ## [29] -12.78458 -13.02320 -12.11269 -11.82824 -12.20960 -12.50708 -11.49175
    ## [36] -11.68654 -12.81725 -10.86039 -11.73381 -11.79526 -12.76963 -11.02074
    ## [43] -11.87297 -12.68378 -12.79340 -11.54172 -12.90829 -11.64100 -12.02963
    ## [50] -12.76300

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.09 0.921

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
    ##    title                            stars              text                     
    ##    <chr>                            <chr>              <chr>                    
    ##  1 no brainer                       5.0 out of 5 stars "\n  watched this with m…
    ##  2 Yeah., it was pretty good.       5.0 out of 5 stars "\n  Yeah, it was pretty…
    ##  3 Love it                          5.0 out of 5 stars "\n  Didn't like this wh…
    ##  4 it was                           5.0 out of 5 stars "\n  mad good yo\n"      
    ##  5 Fun!                             4.0 out of 5 stars "\n  Fun and entertainin…
    ##  6 Vintage                          5.0 out of 5 stars "\n  Easy to order. I th…
    ##  7 too many commercials             1.0 out of 5 stars "\n  5 minutes into the …
    ##  8 this film is so good!            5.0 out of 5 stars "\n  VOTE FOR PEDRO!\n"  
    ##  9 Good movie                       5.0 out of 5 stars "\n  Weird story, goofy …
    ## 10 I Just everyone to know this.... 5.0 out of 5 stars "\n  VOTE FOR PEDRO !!!!…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  2 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  3 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  4 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  5 Painful                                               1.0 ou… "\n  I think I…
    ##  6 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  7 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  8 Cult Classic                                          5.0 ou… "\n  Watched i…
    ##  9 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## 10 Good funny                                            3.0 ou… "\n  Would rec…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Not available w/in 48 hour window           1.0 out of 5 stars "\n  I couldn…
    ##  2 Your mom went to college.                   5.0 out of 5 stars "\n  Classic …
    ##  3 Very funny movie                            5.0 out of 5 stars "\n  I watch …
    ##  4 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  5 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  6 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  7 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  8 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  9 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ## 10 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 Hilarious                                     5.0 out of 5 stars "\n  Funny\…
    ##  2 Love it                                       5.0 out of 5 stars "\n  What o…
    ##  3 WORTH IT!                                     5.0 out of 5 stars "\n  It's t…
    ##  4 Funny movie.                                  5.0 out of 5 stars "\n  Great …
    ##  5 Best movie ever!                              5.0 out of 5 stars "\n  Got th…
    ##  6 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  7 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  8 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  9 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ## 10 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                           stars              text                      
    ##    <chr>                           <chr>              <chr>                     
    ##  1 Perfect                         5.0 out of 5 stars "\n  Exactly what I asked…
    ##  2 Love this movie!                5.0 out of 5 stars "\n  Great movie and sent…
    ##  3 Love it                         5.0 out of 5 stars "\n  Love this movie. How…
    ##  4 As described                    3.0 out of 5 stars "\n  Book is as described…
    ##  5 GOSH!!!                         5.0 out of 5 stars "\n  Just watch the movie…
    ##  6 Watch it right now              5.0 out of 5 stars "\n  You need to watch th…
    ##  7 At this point it’s an addiction 5.0 out of 5 stars "\n  I watch this movie w…
    ##  8 💕                              5.0 out of 5 stars "\n  Hands down, one of m…
    ##  9 Good dumb movie                 5.0 out of 5 stars "\n  I really wanted to s…
    ## 10 funny                           5.0 out of 5 stars "\n  so funny and inventi…

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
    ##    title                            stars              text                     
    ##    <chr>                            <chr>              <chr>                    
    ##  1 no brainer                       5.0 out of 5 stars "\n  watched this with m…
    ##  2 Yeah., it was pretty good.       5.0 out of 5 stars "\n  Yeah, it was pretty…
    ##  3 Love it                          5.0 out of 5 stars "\n  Didn't like this wh…
    ##  4 it was                           5.0 out of 5 stars "\n  mad good yo\n"      
    ##  5 Fun!                             4.0 out of 5 stars "\n  Fun and entertainin…
    ##  6 Vintage                          5.0 out of 5 stars "\n  Easy to order. I th…
    ##  7 too many commercials             1.0 out of 5 stars "\n  5 minutes into the …
    ##  8 this film is so good!            5.0 out of 5 stars "\n  VOTE FOR PEDRO!\n"  
    ##  9 Good movie                       5.0 out of 5 stars "\n  Weird story, goofy …
    ## 10 I Just everyone to know this.... 5.0 out of 5 stars "\n  VOTE FOR PEDRO !!!!…
    ## # … with 40 more rows
