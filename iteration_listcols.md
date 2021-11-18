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
    ## -4.02873 -0.64249 -0.01687 -0.02347  0.64855  3.15221

``` r
l[["summary"]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -4.02873 -0.64249 -0.01687 -0.02347  0.64855  3.15221

``` r
l$summary
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -4.02873 -0.64249 -0.01687 -0.02347  0.64855  3.15221

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
    ## 1  1.90  1.08

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
    ##  [1] 3.36418865 1.73892569 2.28235385 4.07868442 2.25275511 0.85568315
    ##  [7] 1.13970157 0.42433113 0.33095388 2.59540413 0.81674769 2.43744332
    ## [13] 0.59035655 1.57911000 1.37628267 0.67298344 4.15547728 3.84265803
    ## [19] 1.50606584 2.30987759 2.90353768 1.67187701 1.02899875 1.02634008
    ## [25] 3.49695548 1.46165139 2.72032446 2.76007343 0.30945874 1.30749866
    ## [31] 2.51435521 1.85451016 0.86851869 0.65413749 1.61617485 1.63308667
    ## [37] 3.46828879 1.20303230 3.04610898 1.20316393 3.53200694 3.49964355
    ## [43] 0.08025435 1.67371698 1.93835187 1.91728105 2.61764030 1.84873028
    ## [49] 2.20239550 0.43188284
    ## 
    ## $b
    ##  [1]  3.7893927  3.1024234  3.8103051  3.3465327  2.2594811  7.4010679
    ##  [7]  4.9353167  6.5602507  7.9930049  6.6183194  6.5002394  3.6918212
    ## [13]  6.3956437  6.3350084  0.4722949  8.4661973  1.5142082  7.7158159
    ## [19] 10.6721634  4.5780017  5.7360154  9.0654550  4.1019529  9.6426598
    ## [25]  5.6231492  6.0885443  2.3208493  6.9878763  5.7775560  2.4918126
    ## [31]  7.1464431  7.8778935  2.6462131  2.1809784  5.1092850 10.3822290
    ## [37]  5.0290881 11.4809051  8.2001566  4.0213652  4.6576229  0.9175562
    ## [43]  9.8836576  7.5001514 12.3182875  2.4989109  6.0151093  1.1172847
    ## [49]  3.7862427  4.2099152
    ## 
    ## $c
    ##  [1] 19.88959 20.86141 17.07251 20.35196 20.15757 19.50132 19.93301 20.58554
    ##  [9] 19.20890 21.78559 19.60034 20.69471 21.48385 20.13490 18.52502 19.25412
    ## [17] 20.05562 18.44678 19.14479 18.28815 19.83378 22.55201 20.28393 20.65474
    ## [25] 18.85092 19.45037 20.25439 19.28125 18.64328 19.41936 22.06428 21.83194
    ## [33] 19.67851 18.21596 20.75000 21.67072 19.87162 19.25457 18.88628 22.16266
    ## [41] 20.77142 19.48345 20.00068 19.53813 20.44204 19.44102 20.48631 18.94931
    ## [49] 19.93127 19.88195
    ## 
    ## $d
    ##  [1] -11.90792 -12.19078 -11.35710 -12.18708 -11.57178 -11.93584 -11.66863
    ##  [8] -12.21452 -12.04518 -12.32836 -11.97340 -11.74252 -12.82856 -11.84969
    ## [15] -11.32466 -12.35643 -11.19432 -12.72788 -12.68921 -12.26363 -11.49407
    ## [22] -11.88643 -12.18511 -10.64639 -11.22215 -12.17764 -12.12785 -12.19875
    ## [29] -11.69424 -11.53890 -11.72422 -12.12696 -12.47129 -11.13992 -11.59778
    ## [36] -12.24677 -11.92786 -12.53792 -11.67549 -11.65708 -12.09459 -12.31890
    ## [43] -11.92608 -11.61228 -12.20185 -13.34293 -12.64055 -12.13537 -12.17112
    ## [50] -12.01363

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.90  1.08

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

update
