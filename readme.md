Exploring [PGA
ShotLink](https://www.pgatour.com/stats/shotlinkintelligence/overview.html)
================

``` r
library(tidyverse)

strokes = read_csv(file = "~/project/data/strokes.csv")

strokes_clean = strokes %>% 
  mutate(result_cut = case_when(to_location_scorer == 'Fairway' ~ 'fairway',
                                to_location_scorer %in% c('Intermediate Rough','Primary Rough') ~ 'rough',
                                to_location_scorer == 'Green' ~ 'green',
                                T ~ 'other'))

intial_plot = strokes_clean %>%
  mutate(hole = factor(hole, ordered = T)) %>% 
  filter(x > 0) %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       color = result_cut)) +
  geom_point() +
  coord_equal(clip = "off") +
  theme(legend.position = "bottom")

intial_plot
```

![](readme_files/figure-gfm/initial_plot-1.png)<!-- -->
