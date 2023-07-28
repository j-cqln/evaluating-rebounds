library(ggstar)
library(gridExtra)
library(pubtheme)

# Plot rink regions with corresponding shot counts
plot.regions <- function(d, unblocked = FALSE) {
  region_outline_color <- '#000000'
  background_color <- '#ffffff'
  gradient_low_color <- '#b0f0ce'
  gradient_high_color <- '#055229'
  
  if (!unblocked) {
    fill_label <- 'Shot attempts'
    
    regions_fill_limits <- c(250, 680)
    regions_fill_breaks <- c(250, 680)
    regions_fill_labels <- c('250', '680')
    
    regions_plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region_shot_count,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region_outline_color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                          limits = regions_fill_limits,
                          breaks = regions_fill_breaks,
                          labels = regions_fill_labels,
                          oob = squish) +
      labs(title = 'Rink regions',
           subtitle = 'Data plotted by region in all location visualizations',
           caption = 'Created by github.com/j-cqln',
           fill = fill_label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            legend.position = 'top',
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
            legend.direction = 'horizontal',
            legend.justification = c(0, 0),
            legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
      guides(fill = guide_colorbar(order = 2,
                                   title.position = 'top',
                                   title.hjust = 0))
  } else {
    fill_label <- 'Unblocked shot attempts'
    
    regions_fill_limits <- c(150, 660)
    regions_fill_breaks <- c(150, 650)
    regions_fill_labels <- c('150', '650')
    
    regions_plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region_shot_count_unblocked,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region_outline_color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                          limits = regions_fill_limits,
                          breaks = regions_fill_breaks,
                          labels = regions_fill_labels,
                          oob = squish) +
      labs(title = 'Rink regions',
           subtitle = 'Data plotted by region in all location visualizations',
           caption = 'Created by github.com/j-cqln',
           fill = fill_label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            legend.position = 'top',
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
            legend.direction = 'horizontal',
            legend.justification = c(0, 0),
            legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
      guides(fill = guide_colorbar(order = 2,
                                   title.position = 'top',
                                   title.hjust = 0))
  }
  
  return(regions_plot)
}

# Plot average xG by region
plot.xg <- function(d, unblocked = FALSE) {
  region_outline_color <- '#000000'
  background_color <- '#ffffff'
  gradient_low_color <- '#b0f0ce'
  gradient_high_color <- '#055229'
  
  if (!unblocked) {
    title <- 'xG, all shot attempts'
    subtitle <- 'Expected goals, model trained on all shot attempts'
    fill_label <- 'xG'
    
    regions_fill_limits <- c(0.014, 0.16)
    regions_fill_breaks <- c(0.015, 0.15)
    regions_fill_labels <- c('0.015', '0.15')
    
    xg_plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region_xgoal_all,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region_outline_color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                          limits = regions_fill_limits,
                          breaks = regions_fill_breaks,
                          labels = regions_fill_labels,
                          oob = squish) +
      labs(title = title,
           subtitle = subtitle,
           caption = 'Created by github.com/j-cqln',
           fill = fill_label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            legend.position = 'top',
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
            legend.direction = 'horizontal',
            legend.justification = c(0, 0),
            legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
      guides(fill = guide_colorbar(order = 2,
                                   title.position = 'top',
                                   title.hjust = 0))
  } else {
    title <- 'xG, unblocked shot attempts'
    subtitle <- 'Expected goals, model trained on unblocked shots'
    fill_label <- 'xG'
    
    regions_fill_limits <- c(0.014, 0.16)
    regions_fill_breaks <- c(0.015, 0.15)
    regions_fill_labels <- c('0.015', '0.15')
    
    xg_plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region_xgoal_unblocked,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region_outline_color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                          limits = regions_fill_limits,
                          breaks = regions_fill_breaks,
                          labels = regions_fill_labels,
                          oob = squish) +
      labs(title = title,
           subtitle = subtitle,
           caption = 'Created by github.com/j-cqln',
           fill = fill_label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background_color),
            panel.background = element_rect(fill = background_color),
            legend.background = element_rect(fill = background_color),
            legend.position = 'top',
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
            legend.direction = 'horizontal',
            legend.justification = c(0, 0),
            legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
      guides(fill = guide_colorbar(order = 2,
                                   title.position = 'top',
                                   title.hjust = 0))
  }
  
  return(xg_plot)
}

# Plot rebound value
plot.rebounds1 <- function(d, unblocked = FALSE) {
  background_color <- '#ffffff'
  gradient_low_color <- '#bedceb'
  gradient_high_color <- '#004b71'
  
  if (!unblocked) {
    size_label <- 'Shot attempts'
    
    # 1
    rebounds_plot_fill_limits <- c(0.2, 0.45)
    rebounds_plot_fill_breaks <- c(0.25, 0.45)
    rebounds_plot_fill_labels <- c('0.25', '0.45')
    
    rebounds_plot_size_limits <- c(0, 45)
    rebounds_plot_size_breaks <- c(5, 20, 35)
    rebounds_plot_size_labels <- c('5', '20', '35+')
    
    # 2
    rebound_shots_plot_fill_limits <- c(0.015, 0.075)
    rebound_shots_plot_fill_breaks <- c(0.015, 0.075)
    rebound_shots_plot_fill_labels <- c('0.015', '0.075')
    
    rebound_shots_plot_size_limits <- c(0, 15)
    rebound_shots_plot_size_breaks <- c(5, 10, 15)
    rebound_shots_plot_size_labels <- c('5', '10', '15+')
    
    # 3
    xg_plot_fill_limits <- c(0.08, 0.18)
    xg_plot_fill_breaks <- c(0.08, 0.18)
    xg_plot_fill_labels <- c('0.08', '0.18')
    
    xg_plot_size_limits <- c(0, 6)
    xg_plot_size_breaks <- c(1, 3, 5)
    xg_plot_size_labels <- c('1', '3', '5+')
    
    # 4
    rebounds_total_plot_fill_limits <- c(0.0005, 0.006)
    rebounds_total_plot_fill_breaks <- c(0.0005, 0.005)
    rebounds_total_plot_fill_labels <- c('0.0005', '0.005')
    
    rebounds_total_plot_size_limits <- c(0, 45)
    rebounds_total_plot_size_breaks <- c(5, 20, 35)
    rebounds_total_plot_size_labels <- c('5', '20', '35+')
    
  } else {
    size_label <- 'Unblocked shot attempts'
    
    # 1
    rebounds_plot_fill_limits <- c(0.35, 0.55)
    rebounds_plot_fill_breaks <- c(0.35, 0.55)
    rebounds_plot_fill_labels <- c('0.35', '0.55')
    
    rebounds_plot_size_limits <- c(0, 45)
    rebounds_plot_size_breaks <- c(5, 20, 35)
    rebounds_plot_size_labels <- c('5', '20', '35+')
    
    # 2
    rebound_shots_plot_fill_limits <- c(0.03, 0.08)
    rebound_shots_plot_fill_breaks <- c(0.03, 0.08)
    rebound_shots_plot_fill_labels <- c('0.03', '0.08')
    
    rebound_shots_plot_size_limits <- c(0, 15)
    rebound_shots_plot_size_breaks <- c(5, 10, 15)
    rebound_shots_plot_size_labels <- c('5', '10', '15+')
    
    # 3
    xg_plot_fill_limits <- c(0.08, 0.18)
    xg_plot_fill_breaks <- c(0.08, 0.18)
    xg_plot_fill_labels <- c('0.08', '0.18')
    
    xg_plot_size_limits <- c(0, 6)
    xg_plot_size_breaks <- c(1, 3, 5)
    xg_plot_size_labels <- c('1', '3', '5+')
    
    # 4
    rebounds_total_plot_fill_limits <- c(0.001, 0.007)
    rebounds_total_plot_fill_breaks <- c(0.001, 0.006)
    rebounds_total_plot_fill_labels <- c('0.001', '0.006')
    
    rebounds_total_plot_size_limits <- c(0, 45)
    rebounds_total_plot_size_breaks <- c(5, 20, 35)
    rebounds_total_plot_size_labels <- c('5', '20', '35+')
  }
  
  # 1
  shots <- get.shots(d)
  shots_hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots_hexbin_df <- data.frame(hexbin::hcell2xy(shots_hexbin),
                                cell = shots_hexbin@cell,
                                count = shots_hexbin@count)
  shots$cell <- shots_hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              rebound_prob_plot = ifelse(unblocked, mean(region_rebound_prob_unblocked), mean(region_rebound_prob))) %>%
    ungroup() %>%
    right_join(shots_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, rebound_prob_plot)
  
  rebounds_plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = rebound_prob_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                        limits = rebounds_plot_fill_limits,
                        breaks = rebounds_plot_fill_breaks,
                        labels = rebounds_plot_fill_labels) +
    scale_size_area(limits = rebounds_plot_size_limits,
                    breaks = rebounds_plot_size_breaks,
                    labels = rebounds_plot_size_labels,
                    oob = squish) +
    labs(title = 'Shots generating rebound',
         subtitle = 'Probability of shot creating rebound',
         caption = 'Created by github.com/j-cqln',
         fill = 'P(rebound)',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  # 2
  rebounds <- get.shots.generating.rebounds(d)
  rebounds_hexbin <- hexbin::hexbin(rebounds$x, rebounds$y, xbins = 21, IDs = TRUE)
  rebounds_hexbin_df <- data.frame(hexbin::hcell2xy(rebounds_hexbin),
                                   cell = rebounds_hexbin@cell,
                                   count = rebounds_hexbin@count)
  rebounds$cell <- rebounds_hexbin@cID
  
  rebounds <- rebounds %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              rebound_shot_prob_plot = ifelse(unblocked, mean(region_rebound_shot_prob_unblocked), mean(region_rebound_shot_prob))) %>%
    ungroup() %>%
    right_join(rebounds_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, rebound_shot_prob_plot)
  
  rebound_shots_plot <- rink +
    geom_star(data = rebounds,
              aes(x = x, y = y,
                  fill = rebound_shot_prob_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                        limits = rebound_shots_plot_fill_limits,
                        breaks = rebound_shots_plot_fill_breaks,
                        labels = rebound_shots_plot_fill_labels) +
    scale_size_area(limits = rebound_shots_plot_size_limits,
                    breaks = rebound_shots_plot_size_breaks,
                    labels = rebound_shots_plot_size_labels,
                    oob = squish) +
    labs(title = 'Shots generating rebound shot',
         subtitle = 'Probability of shot creating rebound shot from rebound',
         caption = '',
         fill = 'P(rebound shot | rebound)',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  # 3
  rebound_shots <- get.shots.generating.rebound.shots(d)
  rebound_shots_hexbin <- hexbin::hexbin(rebound_shots$x, rebound_shots$y, xbins = 21, IDs = TRUE)
  rebound_shots_hexbin_df <- data.frame(hexbin::hcell2xy(rebound_shots_hexbin),
                                        cell = rebound_shots_hexbin@cell,
                                        count = rebound_shots_hexbin@count)
  rebound_shots$cell <- rebound_shots_hexbin@cID
  
  rebound_shots <- rebound_shots %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              xg_plot = ifelse(unblocked, mean(region_rebound_shot_xg_unblocked), mean(region_rebound_shot_xg))) %>%
    ungroup() %>%
    right_join(rebound_shots_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, xg_plot)
  
  xg_plot <- rink +
    geom_star(data = rebound_shots,
              aes(x = x, y = y,
                  fill = xg_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                        limits = xg_plot_fill_limits,
                        breaks = xg_plot_fill_breaks,
                        labels = xg_plot_fill_labels) +
    scale_size_area(limits = xg_plot_size_limits,
                    breaks = xg_plot_size_breaks,
                    labels = xg_plot_size_labels,
                    oob = squish) +
    labs(title = 'Isolated xG of rebound shots',
         subtitle = 'Rebound shot goal probability ignoring last event',
         caption = '',
         fill = 'Isolated rebound shot xG',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  # 4
  rebounds_total <- get.shots(d)
  rebounds_total_hexbin <- hexbin::hexbin(rebounds_total$x, rebounds_total$y, xbins = 21, IDs = TRUE)
  rebounds_total_hexbin_df <- data.frame(hexbin::hcell2xy(rebounds_total_hexbin),
                                         cell = rebounds_total_hexbin@cell,
                                         count = rebounds_total_hexbin@count)
  rebounds_total$cell <- rebounds_total_hexbin@cID
  
  rebounds_total <- rebounds_total %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              rebounds_total_plot = ifelse(unblocked, mean(region_rebound_total_unblocked), mean(region_rebound_total))) %>%
    ungroup() %>%
    right_join(rebounds_total_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, rebounds_total_plot)
  
  rebounds_total_plot <- rink +
    geom_star(data = rebounds_total,
              aes(x = x, y = y,
                  fill = rebounds_total_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                        limits = rebounds_total_plot_fill_limits,
                        breaks = rebounds_total_plot_fill_breaks,
                        labels = rebounds_total_plot_fill_labels) +
    scale_size_area(limits = rebounds_total_plot_size_limits,
                    breaks = rebounds_total_plot_size_breaks,
                    labels = rebounds_total_plot_size_labels,
                    oob = squish) +
    labs(title = 'Overall \'rebound value\' of shots',
         subtitle = 'Measure of original shot\'s isolated rebound quality',
         caption = '',
         fill = 'Rebound value',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  overall_rebounds <- grid.arrange(rebounds_plot,
                                   rebound_shots_plot,
                                   xg_plot,
                                   rebounds_total_plot,
                                   ncol = 4)
  return(overall_rebounds)
}

# Plot xG* and xG comparison
plot.rebounds2 <- function(d, unblocked = FALSE) {
  background_color <- '#ffffff'
  gradient_low_color <- '#bedceb'
  gradient_high_color <- '#004b71'
  diff_gradient_low_color <- '#c8213e'
  diff_gradient_mid_color <- '#d6d6d6'
  diff_gradient_high_color <- '#3597c9'
  
  if (!unblocked) {
    size_label <- 'Shot attempts'
    
    # 1
    xg_no_last_plot_fill_limits <- c(0.013, 0.16)
    xg_no_last_plot_fill_breaks <- c(0.013, 0.16)
    xg_no_last_plot_fill_labels <- c('0.013', '0.16')
    
    xg_no_last_plot_size_limits <- c(0, 45)
    xg_no_last_plot_size_breaks <- c(5, 20, 35)
    xg_no_last_plot_size_labels <- c('5', '20', '35+')
    
    # 2
    rebounds_total_plot_fill_limits <- c(0.0005, 0.006)
    rebounds_total_plot_fill_breaks <- c(0.0005, 0.005)
    rebounds_total_plot_fill_labels <- c('0.0005', '0.005')
    
    rebounds_total_plot_size_limits <- c(0, 45)
    rebounds_total_plot_size_breaks <- c(5, 20, 35)
    rebounds_total_plot_size_labels <- c('5', '20', '35+')
    
    # 3
    sum_plot_fill_limits <- c(0.013, 0.16)
    sum_plot_fill_breaks <- c(0.013, 0.16)
    sum_plot_fill_labels <- c('0.013', '0.16')
    
    sum_plot_size_limits <- c(0, 45)
    sum_plot_size_breaks <- c(5, 20, 35)
    sum_plot_size_labels <- c('5', '20', '35+')
    
    # 4
    diff_plot_fill_limits <- c(-0.006, 0.006)
    diff_plot_fill_breaks <- c(-0.006, 0.006)
    diff_plot_fill_labels <- c('-0.006', '0.006')
    
    diff_plot_size_limits <- c(0, 45)
    diff_plot_size_breaks <- c(5, 20, 35)
    diff_plot_size_labels <- c('5', '20', '35+')
    
  } else {
    size_label <- 'Unblocked shot attempts'
    
    # 1
    xg_no_last_plot_fill_limits <- c(0.018, 0.16)
    xg_no_last_plot_fill_breaks <- c(0.018, 0.16)
    xg_no_last_plot_fill_labels <- c('0.018', '0.16')
    
    xg_no_last_plot_size_limits <- c(0, 45)
    xg_no_last_plot_size_breaks <- c(5, 20, 35)
    xg_no_last_plot_size_labels <- c('5', '20', '35+')
    
    # 2
    rebounds_total_plot_fill_limits <- c(0.001, 0.007)
    rebounds_total_plot_fill_breaks <- c(0.001, 0.007)
    rebounds_total_plot_fill_labels <- c('0.001', '0.007')
    
    rebounds_total_plot_size_limits <- c(0, 45)
    rebounds_total_plot_size_breaks <- c(5, 20, 35)
    rebounds_total_plot_size_labels <- c('5', '20', '35+')
    
    # 3
    sum_plot_fill_limits <- c(0.018, 0.16)
    sum_plot_fill_breaks <- c(0.018, 0.16)
    sum_plot_fill_labels <- c('0.018', '0.16')
    
    sum_plot_size_limits <- c(0, 45)
    sum_plot_size_breaks <- c(5, 20, 35)
    sum_plot_size_labels <- c('5', '20', '35+')
    
    # 4
    diff_plot_fill_limits <- c(-0.006, 0.006)
    diff_plot_fill_breaks <- c(-0.006, 0.006)
    diff_plot_fill_labels <- c('-0.006', '0.006')
    
    diff_plot_size_limits <- c(0, 45)
    diff_plot_size_breaks <- c(5, 20, 35)
    diff_plot_size_labels <- c('5', '20', '35+')
  }
  
  shots <- get.shots(d)
  shots_hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots_hexbin_df <- data.frame(hexbin::hcell2xy(shots_hexbin),
                                cell = shots_hexbin@cell,
                                count = shots_hexbin@count)
  shots$cell <- shots_hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              xg_no_last_plot = ifelse(unblocked,
                                       mean(region_xgoal_unblocked_no_last),
                                       mean(region_xgoal_all_no_last)),
              rebounds_total_plot = ifelse(unblocked,
                                           mean(region_rebound_total_unblocked),
                                           mean(region_rebound_total)),
              sum_plot = ifelse(unblocked,
                                mean(region_xgoal_unblocked_no_last) + mean(region_rebound_total_unblocked),
                                mean(region_xgoal_all_no_last) + mean(region_rebound_total)),
              diff_plot = ifelse(unblocked,
                                 mean(region_xgoal_unblocked_no_last) + mean(region_rebound_total_unblocked) - mean(region_xgoal_unblocked),
                                 mean(region_xgoal_all_no_last) + mean(region_rebound_total) - mean(region_xgoal_all))) %>%
    ungroup() %>%
    right_join(shots_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, xg_no_last_plot, rebounds_total_plot, sum_plot, diff_plot)
  
  # 1
  xg_no_last_plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = xg_no_last_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                        limits = xg_no_last_plot_fill_limits,
                        breaks = xg_no_last_plot_fill_breaks,
                        labels = xg_no_last_plot_fill_labels) +
    scale_size_area(limits = xg_no_last_plot_size_limits,
                    breaks = xg_no_last_plot_size_breaks,
                    labels = xg_no_last_plot_size_labels,
                    oob = squish) +
    labs(title = 'Isolated xG of original shots',
         subtitle = 'Expected goals of original shots, ignoring last event',
         caption = 'Created by github.com/j-cqln',
         fill = 'Isolated shot xG',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  # 2
  rebounds_total_plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = rebounds_total_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                        limits = rebounds_total_plot_fill_limits,
                        breaks = rebounds_total_plot_fill_breaks,
                        labels = rebounds_total_plot_fill_labels) +
    scale_size_area(limits = rebounds_total_plot_size_limits,
                    breaks = rebounds_total_plot_size_breaks,
                    labels = rebounds_total_plot_size_labels,
                    oob = squish) +
    labs(title = 'Overall \'rebound value\' of shots',
         subtitle = 'Measure of original shot\'s isolated rebound quality',
         caption = '',
         fill = 'Rebound value',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  # 3
  sum_plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = sum_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color, high = gradient_high_color, na.value = NA,
                        limits = sum_plot_fill_limits,
                        breaks = sum_plot_fill_breaks,
                        labels = sum_plot_fill_labels) +
    scale_size_area(limits = sum_plot_size_limits,
                    breaks = sum_plot_size_breaks,
                    labels = sum_plot_size_labels,
                    oob = squish) +
    labs(title = 'Isolated xG with rebound value (xG*)',
         subtitle = 'Sum of isolated xG and rebound value (left 2 plots)',
         caption = '',
         fill = 'Isolated xG + rebound value',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  # 4
  diff_plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = diff_plot,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient2(low = diff_gradient_low_color,
                         mid = diff_gradient_mid_color,
                         high = diff_gradient_high_color,
                         na.value = NA,
                         limits = diff_plot_fill_limits,
                         breaks = diff_plot_fill_breaks,
                         labels = diff_plot_fill_labels) +
    scale_size_area(limits = diff_plot_size_limits,
                    breaks = diff_plot_size_breaks,
                    labels = diff_plot_size_labels,
                    oob = squish) +
    labs(title = 'xG* vs xG with last event',
         subtitle = 'Difference between the two models',
         caption = '',
         fill = 'xG* - xG',
         size = size_label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 20, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  xg_rebounds <- grid.arrange(xg_no_last_plot,
                              rebounds_total_plot,
                              sum_plot,
                              diff_plot,
                              ncol = 4)
  
  return(xg_rebounds)
}

# Plot passes
plot.passes <- function(d, players) {
  background_color <- '#ffffff'
  
  d <- d %>% filter(adj_x > 0) %>% select(x, y, danger)
  
  low_danger_passes_plot <- rink +
    geom_density_2d_filled(data = d %>% filter(danger == 'low'),
                           aes(x = x, y = y),
                           alpha = 0.6,
                           bins = 9,
                           show.legend = FALSE) +
    scale_fill_brewer(palette = 'BuGn') +
    labs(title = 'Passes leading to low danger shot attempts',
         subtitle = 'Darker green indicates higher pass density',
         caption = 'Created by github.com/j-cqln') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color))
  
  high_danger_passes_plot <- rink +
    geom_density_2d_filled(data = d %>% filter(danger == 'high'),
                           aes(x = x, y = y),
                           alpha = 0.6,
                           bins = 9,
                           show.legend = FALSE) +
    scale_fill_brewer(palette = 'BuGn') +
    labs(title = 'Passes leading to high danger shot attempts',
         subtitle = 'Darker green indicates higher pass density',
         caption = '') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color))
  
  players <- players %>%
    arrange(desc(count)) %>%
    mutate(id = row_number()) %>%
    filter(id < 21) %>%
    arrange(count) %>%
    mutate(player = factor(player, player, ordered = TRUE))
  
  players_plot <- ggplot(data = players,
                         aes(x = count, y = player)) +
    geom_segment(aes(x = 5, xend = count,
                     y = player, yend = player,
                     color = position),
                 show.legend = FALSE) +
    geom_point(aes(color = position),
               show.legend = FALSE) +
    scale_color_manual(breaks = c('F', 'D'),
                       values = c('F' = pubred, 'D' = pubblue)) +
    geom_text(aes(label = count), hjust = -0.3) +
    labs(title = 'Top 20 players by passes leading to high danger shots',
         subtitle = 'Red indicates forwards, blue indicates defense',
         # caption  = '',
         x = 'Passes leading to high danger shots',
         y = NULL) +
    xlim(c(5, 25)) +
    theme_pub(type = 'pop', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color))
  
  passes_plots <- grid.arrange(low_danger_passes_plot,
                               high_danger_passes_plot,
                               players_plot,
                               ncol = 3)
  
  return(passes_plots)
}

# Plot zone entries
plot.ozone.entries <- function(d, players) {
  background_color <- '#ffffff'
  
  ozone_entries_plot <- rink +
    geom_density_2d_filled(data = d,
                           aes(x = x, y = y),
                           alpha = 0.6,
                           bins = 9,
                           show.legend = FALSE) +
    scale_fill_brewer(palette = 'BuGn') +
    labs(title = 'Successful offensive zone entries',
         subtitle = 'Darker green indicates higher success density, bottom is o-zone',
         caption = 'Created by github.com/j-cqln',
         x = 'east - west (along blue line)', 
         y = 'o-zone - d-zone') +
    coord_cartesian(ylim = c(-30, -20),
                    xlim = c(-42.5, 42.5)) +
    theme_pub(base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  players <- players %>%
    top_n(count_per_60, n = 20) %>%
    arrange(count_per_60)
  
  players <- players %>%
    distinct(player, .keep_all = TRUE)
  
  players$player = factor(players$player, players$player, ordered = TRUE)
  
  players_plot <- ggplot(data = players,
                         aes(x = count_per_60, y = player)) +
    geom_segment(aes(x = 6, xend = count_per_60,
                     y = player, yend = player,
                     color = position),
                 show.legend = FALSE) +
    geom_point(aes(color = position),
               show.legend = FALSE) +
    scale_color_manual(breaks = c('F', 'D'),
                       values = c('F' = pubred, 'D' = pubblue)) +
    geom_text(aes(label = round(count_per_60, 1)), hjust = -0.3) +
    labs(title = 'Top 20 players by successful offensive zone entries/60',
         subtitle = 'Red indicates forwards, blue indicates defense',
         # caption  = '',
         x = 'Successful offensive zone entries per 60 minutes played',
         y = NULL) +
    xlim(c(6, 26)) +
    theme_pub(type = 'pop', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color))
  
  ozone_entries_plots <- list(ozone_entries_plot, players_plot)
  
  return(ozone_entries_plots)
}