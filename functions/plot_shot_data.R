plot_shot_data = function(shot_data) {
  
  result_cut_colors = c('bunker'='tan',
                        'fairway'='green',
                        'green'='lightgreen',
                        'rough'='darkgreen',
                        'other'='red')
  
  shot_data %>% 
    ggplot(mapping = aes(x = x,
                         y = y,
                         color = result_cut)) +
    geom_point() +
    scale_color_manual(values = result_cut_colors) +
    coord_equal() 

}

