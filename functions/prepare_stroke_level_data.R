prepare_stroke_level_data = function() {
  
  require(tidyverse)
  
  require(janitor)
  
  stroke_level_import = read.delim(file = "~/project/data/sample_download/StrokeLevelTOURChamp.txt")
  
  stroke_level_clean_names = stroke_level_import %>% 
    clean_names()
  
  stroke_level = stroke_level_clean_names %>% 
    mutate(x = str_replace_all(string = x_coordinate,
                               pattern = ",",
                               replacement = "")) %>%
    mutate(y = str_replace_all(string = y_coordinate,
                               pattern = ",",
                               replacement = "")) %>% 
    mutate(z = str_replace_all(string = z_coordinate,
                               pattern = ",",
                               replacement = "")) %>% 
    mutate(x = as.numeric(x),
           y = as.numeric(y),
           z = as.numeric(z)) %>% 
    select(-x_coordinate, 
           -y_coordinate, 
           -z_coordinate) %>% 
    mutate(x = ifelse(x == 0,NA,x)) %>% 
    mutate(y = ifelse(y == 0,NA,y)) %>% 
    mutate(z = ifelse(z == 0,NA,z)) %>% 
    mutate(result_cut = case_when(to_location_scorer == 'Fairway' ~ 'fairway',
                                  to_location_scorer %in% c('Fairway Bunker', 'Green Side Bunker') ~ 'bunker',
                                  to_location_scorer %in% c('Intermediate Rough','Primary Rough') ~ 'rough',
                                  to_location_scorer == 'Green' ~ 'green',
                                  is.na(x) ~ 'hole',
                                  T ~ 'other')) %>% 
    mutate(hole = factor(hole, ordered = T))
  
  return(stroke_level)
  
}
