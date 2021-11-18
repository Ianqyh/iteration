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
    ## -3.065767 -0.689408  0.000852  0.019523  0.716991  3.650081

``` r
l[["summary"]]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.065767 -0.689408  0.000852  0.019523  0.716991  3.650081

``` r
l$summary
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.065767 -0.689408  0.000852  0.019523  0.716991  3.650081

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
    ## 1  2.03 0.927

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
    ##  [1] 0.5955044 1.2529975 2.5476786 3.0394531 1.0684287 0.7008615 3.6646513
    ##  [8] 1.2259330 1.4664662 1.3115582 1.2682362 2.0985510 2.9848895 2.4894000
    ## [15] 1.9297798 2.6187696 4.1815948 2.0992261 2.6629886 4.0350045 0.4823002
    ## [22] 1.8709644 3.0797797 1.5912130 1.5118907 2.2174522 2.5800345 3.2076987
    ## [29] 1.5191280 1.3197644 3.4087103 2.2781149 2.7050268 2.5867564 1.9476220
    ## [36] 0.9495357 1.4273902 1.4730027 2.6007106 1.1784130 2.1894968 2.4360671
    ## [43] 0.9746728 1.8548937 1.3177993 1.3415014 1.6836454 0.3037807 2.7703970
    ## [50] 3.4850650
    ## 
    ## $b
    ##  [1]  2.30580900  6.22755354  4.26152196  7.60867892  7.75429625  3.19023746
    ##  [7]  5.23762370  0.22662095  8.88671301  1.46071844  4.14583905  7.06898318
    ## [13]  5.74763514  2.07804843  4.11143898  5.75760270  2.49322761  7.78970268
    ## [19]  8.17243172  0.97854593  7.67397946  1.43979049  5.90980671  0.09644283
    ## [25] 10.29250585  7.52756995  1.14779388  1.41077312  3.45937622  7.08554999
    ## [31]  3.18347874  9.83880258  3.82564282  6.82327618  8.16734636  4.52761976
    ## [37] 11.03244906  8.43735894  2.48850780  6.07252732  6.64435764  2.17835782
    ## [43]  7.23516502  0.28245751  1.59741656  7.60661284 -0.27203017  8.16419540
    ## [49]  7.21653270  0.58650110
    ## 
    ## $c
    ##  [1] 22.31561 23.25333 20.85834 19.76390 20.19215 18.82431 19.83655 20.11571
    ##  [9] 18.23044 18.15409 20.45751 19.86306 18.92525 20.14197 20.67561 20.04269
    ## [17] 19.03359 20.07213 20.14623 19.41412 19.34076 19.71915 21.65686 19.11097
    ## [25] 18.55669 21.54704 20.89536 21.69819 19.82073 21.19536 18.90140 18.91172
    ## [33] 20.49287 20.05497 19.44006 19.90262 17.25588 21.38486 19.65221 19.75721
    ## [41] 18.90759 19.13375 18.87270 19.85903 19.78379 21.88766 19.60370 21.43786
    ## [49] 21.37133 20.52450
    ## 
    ## $d
    ##  [1] -11.74027 -11.88236 -11.84662 -11.35471 -12.10194 -12.43486 -12.23851
    ##  [8] -11.49343 -12.32544 -11.98834 -12.43101 -12.61196 -11.56903 -12.62577
    ## [15] -12.78326 -11.85537 -11.87405 -11.69819 -11.97622 -11.90791 -12.20960
    ## [22] -12.81428 -11.09902 -11.90525 -12.63403 -12.02985 -11.68592 -11.38158
    ## [29] -11.52125 -11.83379 -11.76412 -11.77256 -11.84772 -12.00742 -11.66650
    ## [36] -12.26826 -12.39211 -12.16835 -11.53848 -11.50518 -11.55785 -13.09826
    ## [43] -11.14849 -11.91193 -12.03931 -12.83976 -11.85819 -12.06996 -12.17386
    ## [50] -12.09274

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.03 0.927

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

hahaha
