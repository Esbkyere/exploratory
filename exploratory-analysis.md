Exploratory analysis
================
Esther Kyeremah
2023-11-01

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggridges)
```

\##Setting options

``` r
library(tidyverse)

knitr::opts_chunk$set(
  fig.width = 6,
  gif.asp = .6,
  out.width = "90%"
)


theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.comtinuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Load weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10,
month = lubridate::floor_date(date, unit = "month")) %>% 
  select(name, id, everything())
```

    ## using cached file: /Users/ESTER1/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-10-22 18:36:33.146851 (8.534)

    ## file min/max dates: 1869-01-01 / 2023-10-31

    ## using cached file: /Users/ESTER1/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2023-10-22 18:37:04.899569 (3.839)

    ## file min/max dates: 1949-10-01 / 2023-10-31

    ## using cached file: /Users/ESTER1/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-10-22 18:37:14.53003 (0.997)

    ## file min/max dates: 1999-09-01 / 2023-10-31

\##Group BY

``` r
weather_df %>% 
  group_by(name, month)
```

    ## # A tibble: 2,190 × 7
    ## # Groups:   name, month [72]
    ##    name           id          date        prcp  tmax  tmin month     
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl> <date>    
    ##  1 CentralPark_NY USW00094728 2021-01-01   157   4.4   0.6 2021-01-01
    ##  2 CentralPark_NY USW00094728 2021-01-02    13  10.6   2.2 2021-01-01
    ##  3 CentralPark_NY USW00094728 2021-01-03    56   3.3   1.1 2021-01-01
    ##  4 CentralPark_NY USW00094728 2021-01-04     5   6.1   1.7 2021-01-01
    ##  5 CentralPark_NY USW00094728 2021-01-05     0   5.6   2.2 2021-01-01
    ##  6 CentralPark_NY USW00094728 2021-01-06     0   5     1.1 2021-01-01
    ##  7 CentralPark_NY USW00094728 2021-01-07     0   5    -1   2021-01-01
    ##  8 CentralPark_NY USW00094728 2021-01-08     0   2.8  -2.7 2021-01-01
    ##  9 CentralPark_NY USW00094728 2021-01-09     0   2.8  -4.3 2021-01-01
    ## 10 CentralPark_NY USW00094728 2021-01-10     0   5    -1.6 2021-01-01
    ## # ℹ 2,180 more rows

So we see that theyve written groups: name \[3\] under the tibble(easy
to miss).

you can ungroup after doing whatever analysis too.. and you can ungroup
for just one or more variables

``` r
weather_df %>% 
group_by(name, month) %>% 
ungroup(name)
```

    ## # A tibble: 2,190 × 7
    ## # Groups:   month [24]
    ##    name           id          date        prcp  tmax  tmin month     
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl> <date>    
    ##  1 CentralPark_NY USW00094728 2021-01-01   157   4.4   0.6 2021-01-01
    ##  2 CentralPark_NY USW00094728 2021-01-02    13  10.6   2.2 2021-01-01
    ##  3 CentralPark_NY USW00094728 2021-01-03    56   3.3   1.1 2021-01-01
    ##  4 CentralPark_NY USW00094728 2021-01-04     5   6.1   1.7 2021-01-01
    ##  5 CentralPark_NY USW00094728 2021-01-05     0   5.6   2.2 2021-01-01
    ##  6 CentralPark_NY USW00094728 2021-01-06     0   5     1.1 2021-01-01
    ##  7 CentralPark_NY USW00094728 2021-01-07     0   5    -1   2021-01-01
    ##  8 CentralPark_NY USW00094728 2021-01-08     0   2.8  -2.7 2021-01-01
    ##  9 CentralPark_NY USW00094728 2021-01-09     0   2.8  -4.3 2021-01-01
    ## 10 CentralPark_NY USW00094728 2021-01-10     0   5    -1.6 2021-01-01
    ## # ℹ 2,180 more rows

\##counting things

count name or month or both name and month observations

``` r
weather_df %>% 
group_by(name) %>% 
  summarize(n_obs = n())
```

    ## # A tibble: 3 × 2
    ##   name           n_obs
    ##   <chr>          <int>
    ## 1 CentralPark_NY   730
    ## 2 Molokai_HI       730
    ## 3 Waterhole_WA     730

``` r
weather_df %>% 
group_by(month) %>% 
  summarize(n_obsi = n())
```

    ## # A tibble: 24 × 2
    ##    month      n_obsi
    ##    <date>      <int>
    ##  1 2021-01-01     93
    ##  2 2021-02-01     84
    ##  3 2021-03-01     93
    ##  4 2021-04-01     90
    ##  5 2021-05-01     93
    ##  6 2021-06-01     90
    ##  7 2021-07-01     93
    ##  8 2021-08-01     93
    ##  9 2021-09-01     90
    ## 10 2021-10-01     93
    ## # ℹ 14 more rows

``` r
weather_df %>% 
group_by(name, month) %>% 
  summarize(n_obsis = n())
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 72 × 3
    ## # Groups:   name [3]
    ##    name           month      n_obsis
    ##    <chr>          <date>       <int>
    ##  1 CentralPark_NY 2021-01-01      31
    ##  2 CentralPark_NY 2021-02-01      28
    ##  3 CentralPark_NY 2021-03-01      31
    ##  4 CentralPark_NY 2021-04-01      30
    ##  5 CentralPark_NY 2021-05-01      31
    ##  6 CentralPark_NY 2021-06-01      30
    ##  7 CentralPark_NY 2021-07-01      31
    ##  8 CentralPark_NY 2021-08-01      31
    ##  9 CentralPark_NY 2021-09-01      30
    ## 10 CentralPark_NY 2021-10-01      31
    ## # ℹ 62 more rows

we can just use the count function

``` r
weather_df %>% 
  count(name, month, name = "n_obs")
```

    ## # A tibble: 72 × 3
    ##    name           month      n_obs
    ##    <chr>          <date>     <int>
    ##  1 CentralPark_NY 2021-01-01    31
    ##  2 CentralPark_NY 2021-02-01    28
    ##  3 CentralPark_NY 2021-03-01    31
    ##  4 CentralPark_NY 2021-04-01    30
    ##  5 CentralPark_NY 2021-05-01    31
    ##  6 CentralPark_NY 2021-06-01    30
    ##  7 CentralPark_NY 2021-07-01    31
    ##  8 CentralPark_NY 2021-08-01    31
    ##  9 CentralPark_NY 2021-09-01    30
    ## 10 CentralPark_NY 2021-10-01    31
    ## # ℹ 62 more rows

other helpful counters

``` r
weather_df %>% 
group_by(month) %>% 
  summarize(
    n_obs = n(),
    n_days = n_distinct(date))
```

    ## # A tibble: 24 × 3
    ##    month      n_obs n_days
    ##    <date>     <int>  <int>
    ##  1 2021-01-01    93     31
    ##  2 2021-02-01    84     28
    ##  3 2021-03-01    93     31
    ##  4 2021-04-01    90     30
    ##  5 2021-05-01    93     31
    ##  6 2021-06-01    90     30
    ##  7 2021-07-01    93     31
    ##  8 2021-08-01    93     31
    ##  9 2021-09-01    90     30
    ## 10 2021-10-01    93     31
    ## # ℹ 14 more rows

\##2by2 tables

``` r
weather_df %>% 
  filter(name != "Molokai_HI") %>% 
  mutate(
    cold = case_when(
      tmax < 5 ~ "cold",
      tmax >= 5 ~ "not_cold",
      TRUE ~ ""
      )
  ) %>% 
 janitor::tabyl(name, cold)
```

    ##            name cold not_cold emptystring_
    ##  CentralPark_NY   96      634            0
    ##    Waterhole_WA  319      395           16

General summaries

``` r
weather_df %>% 
group_by(month) %>% 
  summarize(
    mean_tmax = mean(tmax)
  )
```

    ## # A tibble: 24 × 2
    ##    month      mean_tmax
    ##    <date>         <dbl>
    ##  1 2021-01-01     10.9 
    ##  2 2021-02-01      9.82
    ##  3 2021-03-01     NA   
    ##  4 2021-04-01     16.8 
    ##  5 2021-05-01     19.6 
    ##  6 2021-06-01     24.3 
    ##  7 2021-07-01     25.2 
    ##  8 2021-08-01     25.2 
    ##  9 2021-09-01     22.4 
    ## 10 2021-10-01     18.2 
    ## # ℹ 14 more rows

there are missing vslues so you can get rid of them

``` r
weather_df %>% 
group_by(month) %>% 
  summarize(
    mean_tmax = mean(tmax, na.rm = TRUE),
    median_tmin = median(tmin, na.rm = TRUE)
  )
```

    ## # A tibble: 24 × 3
    ##    month      mean_tmax median_tmin
    ##    <date>         <dbl>       <dbl>
    ##  1 2021-01-01     10.9         0.6 
    ##  2 2021-02-01      9.82       -1.65
    ##  3 2021-03-01     13.7         5   
    ##  4 2021-04-01     16.8         8.05
    ##  5 2021-05-01     19.6        11.1 
    ##  6 2021-06-01     24.3        17.8 
    ##  7 2021-07-01     25.2        21.1 
    ##  8 2021-08-01     25.2        21.1 
    ##  9 2021-09-01     22.4        17.5 
    ## 10 2021-10-01     18.2        13.9 
    ## # ℹ 14 more rows

THIS IS A DATAFRAME!

``` r
weather_df %>% 
group_by(name, month) %>% 
  summarize(
    mean_tmax = mean(tmax, na.rm = TRUE),
    median_tmin = median(tmin, na.rm = TRUE)
  )
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 72 × 4
    ## # Groups:   name [3]
    ##    name           month      mean_tmax median_tmin
    ##    <chr>          <date>         <dbl>       <dbl>
    ##  1 CentralPark_NY 2021-01-01      4.27       -0.5 
    ##  2 CentralPark_NY 2021-02-01      3.87       -1.85
    ##  3 CentralPark_NY 2021-03-01     12.3         5   
    ##  4 CentralPark_NY 2021-04-01     17.6         8.05
    ##  5 CentralPark_NY 2021-05-01     22.1        11.1 
    ##  6 CentralPark_NY 2021-06-01     28.1        18.0 
    ##  7 CentralPark_NY 2021-07-01     28.4        21.1 
    ##  8 CentralPark_NY 2021-08-01     28.8        22.2 
    ##  9 CentralPark_NY 2021-09-01     24.8        17.5 
    ## 10 CentralPark_NY 2021-10-01     19.9        13.9 
    ## # ℹ 62 more rows

We can make a plot from the summary

``` r
weather_df %>% 
group_by(name, month) %>% 
  summarize(
    mean_tmax = mean(tmax, na.rm = TRUE),
    median_tmin = median(tmin, na.rm = TRUE)
  ) %>% 
ggplot(aes(x = month, y = mean_tmax, color = name)) +
  geom_point() +
  geom_line()  
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

![](exploratory-analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

what is below shows what we would have had if we had just put in our raw
date and tmax..it literally gives us the the data day by day so
depending on what you want to convey in your research

``` r
weather_df %>% 
ggplot(aes(x = date, y = tmax, color = name)) +
  geom_point() +
  geom_line()  
```

    ## Warning: Removed 17 rows containing missing values (`geom_point()`).

    ## Warning: Removed 1 row containing missing values (`geom_line()`).

![](exploratory-analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Suppose you want to summarize many columns

``` r
 weather_df %>% 
group_by(name, month) %>% 
summarize(across(prcp:tmin, mean))
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 72 × 5
    ## # Groups:   name [3]
    ##    name           month       prcp  tmax  tmin
    ##    <chr>          <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY 2021-01-01  18.9  4.27 -1.15
    ##  2 CentralPark_NY 2021-02-01  46.6  3.87 -1.39
    ##  3 CentralPark_NY 2021-03-01  28.0 12.3   3.1 
    ##  4 CentralPark_NY 2021-04-01  22.8 17.6   7.48
    ##  5 CentralPark_NY 2021-05-01  35.7 22.1  12.2 
    ##  6 CentralPark_NY 2021-06-01  22.2 28.1  18.9 
    ##  7 CentralPark_NY 2021-07-01  90.9 28.4  20.6 
    ##  8 CentralPark_NY 2021-08-01  84.5 28.8  21.8 
    ##  9 CentralPark_NY 2021-09-01  84.9 24.8  17.8 
    ## 10 CentralPark_NY 2021-10-01  43.1 19.9  13.4 
    ## # ℹ 62 more rows

Reminder: sometimes your dresults are easier to read in another format

``` r
 weather_df %>% 
group_by(name, month) %>% 
summarize(mean_tmax = mean(tmax)) %>% 
  pivot_wider(
names_from = name,
values_from = mean_tmax) %>% 
knitr::kable(digits = 1)
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

| month      | CentralPark_NY | Molokai_HI | Waterhole_WA |
|:-----------|---------------:|-----------:|-------------:|
| 2021-01-01 |            4.3 |       27.6 |          0.8 |
| 2021-02-01 |            3.9 |       26.4 |         -0.8 |
| 2021-03-01 |           12.3 |       25.9 |           NA |
| 2021-04-01 |           17.6 |       26.6 |          6.1 |
| 2021-05-01 |           22.1 |       28.6 |          8.2 |
| 2021-06-01 |           28.1 |       29.6 |         15.3 |
| 2021-07-01 |           28.4 |       30.0 |         17.3 |
| 2021-08-01 |           28.8 |       29.5 |         17.2 |
| 2021-09-01 |           24.8 |       29.7 |         12.6 |
| 2021-10-01 |           19.9 |       29.1 |          5.5 |
| 2021-11-01 |           11.5 |       28.8 |          3.5 |
| 2021-12-01 |            9.6 |       26.2 |           NA |
| 2022-01-01 |            2.9 |       26.6 |          3.6 |
| 2022-02-01 |            7.7 |       26.8 |           NA |
| 2022-03-01 |           12.0 |       27.7 |          3.4 |
| 2022-04-01 |           15.8 |       27.7 |          2.5 |
| 2022-05-01 |           22.3 |         NA |          5.8 |
| 2022-06-01 |           26.1 |       29.2 |         11.1 |
| 2022-07-01 |           30.7 |       29.5 |         15.9 |
| 2022-08-01 |           30.5 |       30.7 |           NA |
| 2022-09-01 |           24.9 |       30.4 |         15.2 |
| 2022-10-01 |           17.4 |       29.2 |         11.9 |
| 2022-11-01 |           14.0 |       28.0 |          2.1 |
| 2022-12-01 |            6.8 |       27.3 |           NA |
