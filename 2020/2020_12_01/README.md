Untitled
================

``` r
raw_df <- tidytuesdayR::tt_load(2020, week = 49)
```

    ## --- Compiling #TidyTuesday Information for 2020-12-01 ----

    ## --- There is 1 file available ---

    ## --- Starting Download ---

    ## 
    ##  Downloading file 1 of 1: `shelters.csv`

    ## --- Download complete ---

``` r
raw_df <- raw_df$shelters
```

``` r
raw_df
```

    ## # A tibble: 115,916 x 13
    ##       id occupancy_date      organization_na~ shelter_name shelter_address
    ##    <dbl> <dttm>              <chr>            <chr>        <chr>          
    ##  1     1 2017-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ##  2     2 2017-01-01 00:00:00 Christie Ossing~ Christie Os~ 973 Lansdowne ~
    ##  3     3 2017-01-01 00:00:00 Christie Ossing~ Christie Os~ 973 Lansdowne ~
    ##  4     4 2017-01-01 00:00:00 Christie Refuge~ Christie Re~ 43 Christie St~
    ##  5     5 2017-01-01 00:00:00 City of Toronto  Birchmount ~ 1673 Kingston ~
    ##  6     6 2017-01-01 00:00:00 City of Toronto  Birkdale Re~ 1229 Ellesmere~
    ##  7     7 2017-01-01 00:00:00 City of Toronto  Birkdale Re~ 1229 Ellesmere~
    ##  8     8 2017-01-01 00:00:00 City of Toronto  Downsview D~ 1651 Sheppard ~
    ##  9     9 2017-01-01 00:00:00 City of Toronto  Family Resi~ 4222 Kingston ~
    ## 10    10 2017-01-01 00:00:00 City of Toronto  Family Resi~ 4222 Kingston ~
    ## # ... with 115,906 more rows, and 8 more variables: shelter_city <chr>,
    ## #   shelter_province <chr>, shelter_postal_code <chr>, facility_name <chr>,
    ## #   program_name <chr>, sector <chr>, occupancy <dbl>, capacity <dbl>

Not sure what some of the fields refer to. Looks like `id` is in the
dataset 3 times:

``` r
raw_df %>%
  count(id) %>%
  ggplot(aes(x = n)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](D:/Dropbox/ds_projects/tidytuesday/2020/2020_12_01/README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
but it’s not referring to identical shelters/facilities/program names:

``` r
raw_df %>%
  filter(id %in% 1:4) %>%
  arrange(id)
```

    ## # A tibble: 12 x 13
    ##       id occupancy_date      organization_na~ shelter_name shelter_address
    ##    <dbl> <dttm>              <chr>            <chr>        <chr>          
    ##  1     1 2017-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ##  2     1 2018-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ##  3     1 2019-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ##  4     2 2017-01-01 00:00:00 Christie Ossing~ Christie Os~ 973 Lansdowne ~
    ##  5     2 2018-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ##  6     2 2019-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ##  7     3 2017-01-01 00:00:00 Christie Ossing~ Christie Os~ 973 Lansdowne ~
    ##  8     3 2018-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ##  9     3 2019-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ## 10     4 2017-01-01 00:00:00 Christie Refuge~ Christie Re~ 43 Christie St~
    ## 11     4 2018-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ## 12     4 2019-01-01 00:00:00 COSTI Immigrant~ COSTI Recep~ 100 Lippincott~
    ## # ... with 8 more variables: shelter_city <chr>, shelter_province <chr>,
    ## #   shelter_postal_code <chr>, facility_name <chr>, program_name <chr>,
    ## #   sector <chr>, occupancy <dbl>, capacity <dbl>

Ignore for now…

``` r
raw_df %>%
  distinct(sector)
```

    ## # A tibble: 5 x 1
    ##   sector  
    ##   <chr>   
    ## 1 Co-ed   
    ## 2 Men     
    ## 3 Families
    ## 4 Women   
    ## 5 Youth

``` r
raw_df %>%
  mutate(month = month(occupancy_date, label = TRUE),
         year = year(occupancy_date)) %>%
  group_by(month, year, sector) %>%
  summarise(mean_occupancy = mean(occupancy, na.rm = T)) %>%
  ggplot(aes(x = month, y = mean_occupancy, colour = as.factor(sector), group = sector)) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ year) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
```

    ## `summarise()` regrouping output by 'month', 'year' (override with `.groups` argument)

![](D:/Dropbox/ds_projects/tidytuesday/2020/2020_12_01/README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
raw_df %>%
  mutate(month = month(occupancy_date, label = TRUE),
         year = year(occupancy_date)) %>%
  group_by(month, year, sector) %>%
  summarise(mean_occupancy = mean(occupancy, na.rm = T),
            mean_capacity = mean(capacity, na.rm = T),
            pct = mean_occupancy / mean_capacity) %>%
ggplot(aes(x = month, y = pct, colour = as.factor(sector), group = sector)) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ year) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
```

    ## `summarise()` regrouping output by 'month', 'year' (override with `.groups` argument)

![](D:/Dropbox/ds_projects/tidytuesday/2020/2020_12_01/README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
library(ggtext)

raw_df %>%
  mutate(diff = capacity - occupancy) %>%
   mutate(month = month(occupancy_date, label = TRUE),
         year = year(occupancy_date)) %>%
  group_by(month, year, sector) %>%
  summarise(mean_diff = mean(diff, na.rm = T), .groups = "keep") %>%
  ungroup() %>%
  ggplot(aes(x = month, y = mean_diff)) +
  geom_col(fill = "#5D729D", colour = NA) +
  facet_grid(sector ~ year) +
  labs(
    title = "Spare capacity",
    subtitle = "How many spare beds are there?"
  ) +
  theme_minimal() + 
   theme(
     aspect.ratio = 0.3,
     axis.text.x = element_text(family = "Roboto Condensed", size = 11, colour = "grey30", angle = 90, vjust = 0.5),
     axis.text.y = element_text(family = "Roboto Condensed", size = 11, colour = "grey30"),
     plot.title = element_text(family = "Roboto Condensed", size = 18, colour = "#000000", face = "bold"),
     plot.subtitle = element_text(family = "Roboto Condensed", size = 16, colour = "#000000"),
     panel.grid.minor = element_blank(),
     strip.text.x = element_textbox(
      size = 12,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(1, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
   )
   )
```

![](D:/Dropbox/ds_projects/tidytuesday/2020/2020_12_01/README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
