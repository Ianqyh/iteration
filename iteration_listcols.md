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
    ## -3.19346 -0.70103 -0.07378 -0.03814  0.64046  3.05172

``` r
l[["summary"]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.19346 -0.70103 -0.07378 -0.03814  0.64046  3.05172

``` r
l$summary
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.19346 -0.70103 -0.07378 -0.03814  0.64046  3.05172

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
    ## 1  1.66  1.16

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
    ##  [1]  2.23365382  1.93335932 -0.99639324  1.65879859  1.06126073  1.32331414
    ##  [7] -0.10844509  2.53644381  1.44381876  1.05816226  2.40816512  1.48679240
    ## [13] -0.48802635  0.85543172  1.28630537  2.55500751  2.18541276  2.11286513
    ## [19] -0.02667724  2.21156128  0.77906897  0.55897664  1.51085589  1.58509861
    ## [25]  2.20662991  1.11021635  2.23514886  3.85323463  0.95499823  3.21639236
    ## [31]  1.72125440  0.20078719  3.22434228  2.00699409  4.17979916  0.15534149
    ## [37]  3.62865824  1.22800451  3.12644593  1.68121385  2.67854596  0.46064714
    ## [43]  2.66934381  3.28848497  1.25085632  2.28577995  1.80155055 -0.88182634
    ## [49]  1.95835485  1.49308045
    ## 
    ## $b
    ##  [1]  8.325828  5.039004 -1.020709 10.370067  3.583672  4.805540  5.418653
    ##  [8]  6.086550  5.950350 11.363232  7.391060  2.181653  3.590343 -4.085758
    ## [15]  8.914525  4.353723  4.116636 10.089697  4.749022  1.479342  3.180117
    ## [22]  7.854178  6.717541  4.875371  2.797412  2.873464 13.592170  4.657331
    ## [29]  2.387339  8.545811  8.874378  2.273206  3.202811  4.894600  3.718443
    ## [36]  8.548728  2.433948  5.421447  4.859296  8.536731  9.705057  9.141417
    ## [43]  5.777732  1.351100  6.574727 11.779316  6.666368  3.207550  7.566282
    ## [50]  7.469280
    ## 
    ## $c
    ##  [1] 21.50639 20.01824 19.53563 18.27663 19.46357 21.57675 19.36585 21.96501
    ##  [9] 19.91383 21.51729 20.27379 19.54923 21.36908 21.33629 20.90545 21.92551
    ## [17] 20.68711 19.86950 20.26482 20.86338 19.61766 22.30013 19.02622 20.00559
    ## [25] 19.47823 19.16854 21.03818 19.20692 17.71359 21.04036 20.70508 18.47376
    ## [33] 20.25217 21.19610 20.50850 21.18428 21.71874 17.22113 19.64566 22.13567
    ## [41] 20.22644 20.13738 18.98257 19.91457 18.78755 17.80353 21.87713 19.32874
    ## [49] 20.86535 20.20942
    ## 
    ## $d
    ##  [1] -12.83842 -11.66874 -12.22674 -11.00088 -11.64829 -12.29180 -11.85698
    ##  [8] -11.90412 -11.45964 -12.23042 -13.09846 -11.88763 -12.24209 -12.53648
    ## [15] -11.61530 -12.51213 -11.44818 -11.61963 -11.63893 -11.80138 -12.46959
    ## [22] -12.17746 -11.40388 -11.44463 -12.42393 -11.43894 -12.44597 -11.68773
    ## [29] -12.47533 -12.14488 -11.96508 -11.87822 -11.93652 -11.50644 -13.11126
    ## [36] -12.60041 -10.94246 -12.29232 -12.01770 -10.17996 -11.67199 -13.05336
    ## [43] -12.30253 -12.30973 -11.92414 -11.88805 -11.42367 -11.64504 -12.38742
    ## [50] -12.16591

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.66  1.16

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
