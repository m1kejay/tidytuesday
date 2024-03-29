Week 2021/35 Lemurs
================

``` r
extrafont::loadfonts(device = "win")

library(tidyverse)
library(ggthemes)
library(ggtext)

raw_df <-  readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv")
taxonomy <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/taxonomy.csv")
```

``` r
raw_df %>%
  # get rid of irrelevant columns and pull out distinct animals only
  select(-matches("weight|wt|days_before_death|preg|expected_gestation_d|age_category")) %>%
  distinct(across()) %>%
  ggplot(aes(x = age_at_death_y)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  theme_minimal() +
  scale_y_continuous(
    name = "# of Lemurs"
  ) +
  scale_x_continuous(
    name = "Age at death"
  ) +
  theme_minimal()
```

    ## Warning: Removed 827 rows containing non-finite values (stat_bin).

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
raw_df %>%
  left_join(taxonomy, by = "taxon") %>%
  select(-matches("weight|wt|days_before_death|preg|expected_gestation_d")) %>%
  distinct(across()) %>%
  group_by(common_name) %>%
  filter(!is.na(age_at_death_y),
         !is.na(common_name),
         age_category != "IJ", # Exclude infant/juvenile deaths
         #n() >= 10 # Have at least 10 individuals
         ) %>%
  ggplot(aes(x = age_at_death_y, y = fct_reorder(common_name, age_at_death_y, .fun = mean))) +
  geom_point(colour = "#0EA4EA", alpha = 0.1) +
  stat_summary(fun = "mean", geom = "point", na.rm = TRUE, fill = "#FF9935", colour = "#8B541C", pch = 21, size = 3) +
  labs(
    #title = "What are the oldest breeds?",
    title = "What is the <span style='color:#FF9935'>**median age**</span> of <span style='color:#0EA4EA'>**all lemurs**</span> by breed?",
    y = "",
    x = "Age at death"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = 3, size = 0.5),
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 15, hjust = 0.5, margin = margin(0,0,10,0)),
    plot.subtitle = element_markdown(size = 15, hjust  = 0.5, margin = margin(0,0,10,0)),
    axis.title.x = element_markdown(hjust = 0.5)
  ) +
  # Hacky way to get y axis label above breeds
  coord_cartesian(
    clip = "off"
  ) +
  geom_richtext(data = . %>% filter(row_number() == 1), label = "Breed", x = -3.5, y = 26, label.colour = NA)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
library(ggridges)

raw_df %>%
  left_join(taxonomy, by = "taxon") %>%
  select(-matches("weight|wt|days_before_death|preg|expected_gestation_d")) %>%
  distinct(across()) %>%
  group_by(common_name) %>%
  filter(!is.na(age_at_death_y),
         !is.na(common_name),
         age_category != "IJ", # Exclude infant/juvenile deaths
         n() >= 10 # Have at least 10 individuals
         ) %>%
  ggplot(aes(x = age_at_death_y, y = fct_reorder(common_name, age_at_death_y, .fun = mean))) +
  ggridges::geom_density_ridges(
    scale = 0.95,
    size = 0.25,
    rel_min_height = 0.005, 
    jittered_points = TRUE, 
    point_shape = 21, 
    point_size = 3,
    position = position_raincloud(height = 0), 
    fill = "#0EA4EA",
    point_colour = "#FFFFFF",
    alpha = 0.5,
    quantile_lines = TRUE, quantiles = 2,
    vline_colour = "#FF9935",
    vline_size = 1
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    name = "Breed"
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    name = "Age"
  )+
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "What is the <span style='color:#FF9935'>**median age**</span> of <span style='color:#0EA4EA'>**all lemurs**</span> by breed?") +
  theme_ridges() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 15)
    )
```

    ## Picking joint bandwidth of 2.72

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
formatted_df <-
  raw_df %>%
  select(-matches("weight|wt|days_before_death|preg|expected_gestation_d|age_category")) %>%
  distinct(across()) %>%
  slice_max(age_at_death_y, n = 20) %>%
  select(name, dob, dod) %>%
  mutate(
    years_old = as.numeric((dod - dob) / 365),
    name = str_to_sentence(name)
  ) %>%
  pivot_longer(c(dob, dod), names_to = "date_type", values_to = "date") %>%
  arrange(date_type, desc(date))

formatted_df %>%
  ggplot(aes(x = date, y = fct_inorder(name))) +
  geom_line(linetype = 3) +
  geom_point(aes(colour = date_type), size = 5) +
  geom_text(
    data = . %>% filter(date_type == "dod"),
    aes(x = (date + 1500), label = round(years_old, 1)),
    size = 4,
    hjust = "left"
  ) +
  geom_text(
    data = . %>% filter(date_type == "dob"),
    aes(x = (date - 1500), label = name),
    size = 4,
    hjust = "right"
  ) +
  theme_minimal() +
  scale_y_discrete(
    name = "",
    expand = c(0, 0.1)
  ) +
  scale_x_date(
    # expand = c(0, 0.1),
    name = "Date"
  ) +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.y = element_line(linetype = 3),
    legend.position = "none"
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  annotate("text", x = min(formatted_df$date) + 1500, y = 21, label = "Born")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Above is fine, but only includes animals which have died… what about
those that are still alive?

``` r
formatted_df <-
  raw_df %>%
  select(-matches("weight|wt|days_before_death|preg|expected_gestation_d|age_category")) %>%
  distinct(across()) %>%
  select(name, dob, dod, age_at_death_y, age_of_living_y) %>%
  filter(!is.na(age_at_death_y) | !is.na(age_of_living_y)) %>%
  # Select age from either of the two columns age_at_death_y and age_of_living_y
  mutate(age = if_else(is.na(age_at_death_y), age_of_living_y, age_at_death_y)) %>%
  slice_max(age, n = 20) %>%
  mutate(
    name = str_to_sentence(name),
    type = if_else(is.na(dod), "Alive", "Dead"),
    # This is dummy value to make it easier to plot start/end of lines
    dod = if_else(is.na(dod), Sys.Date(), dod)
  ) %>%
  select(-age_at_death_y, -age_of_living_y) %>%
  pivot_longer(c(dob, dod), names_to = "date_type", values_to = "date") %>%
  arrange(type, date_type, desc(date))
```

``` r
# Annotations and labels (Most of this isn't used in current iteration)
ann_text <-
  data.frame(
    type = rep("Dead", 2),
    lab = c("<span style='color:#C72E29'>**Born**</span>", "<span style='color:#016392'>**Died**</span>"),
    x = as.Date(c("1953-01-01", "1974-01-01")),
    xend = as.Date(c(min(formatted_df$date), "1981-03-05")),
    y = c(17.5, 17.5),
    yend = c(16.45, 16.45),
    curv = c(0.5, -0.5),
    col = c("#C72E29", "#016392")
  )

ann_text
```

    ##   type                                         lab          x       xend    y
    ## 1 Dead <span style='color:#C72E29'>**Born**</span> 1953-01-01 1946-10-01 17.5
    ## 2 Dead <span style='color:#016392'>**Died**</span> 1974-01-01 1981-03-05 17.5
    ##    yend curv     col
    ## 1 16.45  0.5 #C72E29
    ## 2 16.45 -0.5 #016392

``` r
formatted_df %>%
  mutate(label = if_else(name == "Pop", glue::glue("{round(age,1)} **years**"), as.character(round(age, 1)))) %>%
  ggplot(aes(x = date, y = fct_inorder(name))) +
  geom_line(linetype = 3, colour = "#5E5C56") +
  geom_point(data = . %>% filter(type == "Dead"), aes(colour = date_type), size = 4) +
  geom_point(data = . %>% filter(type == "Alive" & date != Sys.Date()), aes(colour = date_type), size = 4) +
  facet_grid(fct_rev(type) ~ ., scales = "free", space = "free") +
  geom_richtext(
    data = . %>% filter(date_type == "dod"),
    aes(x = (date + 500), label = label),
    size = 3.5,
    hjust = "left",
    fill = NA,
    label.colour = NA
  ) +
  geom_text(
    data = . %>% filter(date_type == "dob"),
    aes(x = (date - 500), label = name),
    size = 3.5,
    hjust = "right"
  ) +
  # theme_minimal() +
  theme_wsj(title_family = "sans") +
  scale_y_discrete(
    name = "",
    # expand = c(0, 0.1)
  ) +
  scale_x_date(
    # expand = c(0, 0.1),
    name = ""
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  # #https://stackoverflow.com/questions/55627528/how-can-i-pass-individual-curvature-arguments-in-ggplot2-geom-curve-functi
  # lapply(split(ann_text, 1:nrow(ann_text)), function(dat) {
  #   geom_curve(data = dat, aes(x = x, y = y-0.25, xend = xend, yend = yend),
  #              curvature = dat["curv"],
  #              arrow = arrow(length = unit(0.2, "cm")),
  #              lineend = "round"
  #              ) }
  # ) +
  # geom_richtext(
  #   data = ann_text,
  #   aes(x = x, y = y, label = lab),
  #   label.colour = NA,
  #   vjust = 0.5,
  #   fill = "#F8F2E4"
  # ) +
  labs(
    title = "Oldest lemurs at Duke Lemur Center",
    subtitle = "When were the oldest known lemurs <span style='color:#C72E29'>**born**</span> and when did they <span style='color:#016392'>**die**</span>?"
  ) +
  scale_colour_manual(
    values = c("#C72E29", "#016392")
  ) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    plot.title = element_markdown(size = 20, hjust = 0.5),
    plot.subtitle = element_markdown(size = 15, hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_textbox(face = "bold", fill = "#000000") # Fill isn't working...
  )
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Ordered by age:

``` r
formatted_df %>%
  arrange(age) %>%
  mutate(label = if_else(name == "Mercury", glue::glue("{round(age,1)} **years**"), as.character(round(age, 1)))) %>%
  ggplot(aes(x = date, y = fct_inorder(name))) +
  geom_line(linetype = 3) +
  geom_point(data = . %>% filter(type == "Dead"), aes(colour = date_type), size = 4) +
  geom_point(data = . %>% filter(type == "Alive" & date != Sys.Date()), aes(colour = date_type), size = 4) +
  # facet_grid(fct_rev(type) ~ ., scales = "free", space = "free", drop = TRUE) +
  geom_richtext(
    data = . %>% filter(date_type == "dod"),
    aes(x = (date + 500), label = label),
    size = 3.5,
    hjust = "left",
    fill = NA,
    label.colour = NA
  ) +
  geom_text(
    data = . %>% filter(date_type == "dob"),
    aes(x = (date - 500), label = name),
    size = 3.5,
    hjust = "right"
  ) +
  # theme_minimal() +
  theme_wsj(title_family = "sans") +
  scale_y_discrete(
    name = "",
    # expand = c(0, 0.1)
  ) +
  scale_x_date(
    # expand = c(0, 0.1),
    name = ""
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Oldest lemurs at Duke Lemur Center",
    subtitle = "When were the oldest known lemurs <span style='color:#C72E29'>**born**</span> and when did they <span style='color:#016392'>**die**</span>?"
  ) +
  scale_colour_manual(
    values = c("#C72E29", "#016392")
  ) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    plot.title = element_markdown(size = 20),
    plot.subtitle = element_markdown(size = 15),
    strip.text = element_textbox()
  )
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
