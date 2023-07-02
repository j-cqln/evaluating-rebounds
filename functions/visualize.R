library(ggstar)
library(gridExtra)
library(pubtheme)

# Plot rink regions with corresponding shot counts
plot.regions <- function(d, unblocked = FALSE) {
  region.outline.color <- '#000000'
  background.color <- '#ffffff'
  gradient.low.color <- '#b0f0ce'
  gradient.high.color <- '#055229'
  
  if (!unblocked) {
    fill.label <- 'Shot attempts'
    
    regions.fill.limits <- c(250, 680)
    regions.fill.breaks <- c(250, 680)
    regions.fill.labels <- c('250', '680')
    
    regions.plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region.shot.count,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region.outline.color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                          limits = regions.fill.limits,
                          breaks = regions.fill.breaks,
                          labels = regions.fill.labels,
                          oob = squish) +
      labs(title = 'Rink regions',
           subtitle = 'Data plotted by region in all location visualizations',
           caption = 'Created by github.com/j-cqln',
           fill = fill.label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background.color),
            panel.background = element_rect(fill = background.color),
            legend.background = element_rect(fill = background.color),
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
    fill.label <- 'Unblocked shot attempts'
    
    regions.fill.limits <- c(150, 660)
    regions.fill.breaks <- c(150, 650)
    regions.fill.labels <- c('150', '650')
    
    regions.plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region.shot.count.unblocked,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region.outline.color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                          limits = regions.fill.limits,
                          breaks = regions.fill.breaks,
                          labels = regions.fill.labels,
                          oob = squish) +
      labs(title = 'Rink regions',
           subtitle = 'Data plotted by region in all location visualizations',
           caption = 'Created by github.com/j-cqln',
           fill = fill.label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background.color),
            panel.background = element_rect(fill = background.color),
            legend.background = element_rect(fill = background.color),
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
  
  return(regions.plot)
}

# Plot average xG by region
plot.xg <- function(d, unblocked = FALSE) {
  region.outline.color <- '#000000'
  background.color <- '#ffffff'
  gradient.low.color <- '#b0f0ce'
  gradient.high.color <- '#055229'
  
  if (!unblocked) {
    title <- 'xG, all shot attempts'
    subtitle <- 'Expected goals, model trained on all shot attempts'
    fill.label <- 'xG'
    
    regions.fill.limits <- c(0.014, 0.16)
    regions.fill.breaks <- c(0.015, 0.15)
    regions.fill.labels <- c('0.015', '0.15')
    
    xg.plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region.xgoal.all,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region.outline.color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                          limits = regions.fill.limits,
                          breaks = regions.fill.breaks,
                          labels = regions.fill.labels,
                          oob = squish) +
      labs(title = title,
           subtitle = subtitle,
           caption = 'Created by github.com/j-cqln',
           fill = fill.label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background.color),
            panel.background = element_rect(fill = background.color),
            legend.background = element_rect(fill = background.color),
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
    fill.label <- 'xG'
    
    regions.fill.limits <- c(0.014, 0.16)
    regions.fill.breaks <- c(0.015, 0.15)
    regions.fill.labels <- c('0.015', '0.15')
    
    xg.plot <- rink +
      geom_polygon(data = d,
                   aes(x = x, y = y,
                       fill = region.xgoal.unblocked,
                       group = region),
                   alpha = 0.6,
                   size = 0.2,
                   color = region.outline.color,
                   show.legend = TRUE) +
      scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                          limits = regions.fill.limits,
                          breaks = regions.fill.breaks,
                          labels = regions.fill.labels,
                          oob = squish) +
      labs(title = title,
           subtitle = subtitle,
           caption = 'Created by github.com/j-cqln',
           fill = fill.label) +
      ylim(c(-100.1, -24)) +
      xlim(c(-42.6, 42.6)) +
      theme_pub(type = 'map', base_size = 36/3) +
      theme(plot.background = element_rect(fill = background.color),
            panel.background = element_rect(fill = background.color),
            legend.background = element_rect(fill = background.color),
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
  
  return(xg.plot)
}

# Plot rebound value
plot.rebounds1 <- function(d, unblocked = FALSE) {
  background.color <- '#ffffff'
  gradient.low.color <- '#bedceb'
  gradient.high.color <- '#004b71'
  
  if (!unblocked) {
    size.label <- 'Shot attempts'
    
    # 1
    rebounds.plot.fill.limits <- c(0.2, 0.45)
    rebounds.plot.fill.breaks <- c(0.25, 0.45)
    rebounds.plot.fill.labels <- c('0.25', '0.45')
    
    rebounds.plot.size.limits <- c(0, 45)
    rebounds.plot.size.breaks <- c(5, 20, 35)
    rebounds.plot.size.labels <- c('5', '20', '35+')
    
    # 2
    rebound.shots.plot.fill.limits <- c(0.015, 0.075)
    rebound.shots.plot.fill.breaks <- c(0.015, 0.075)
    rebound.shots.plot.fill.labels <- c('0.015', '0.075')
    
    rebound.shots.plot.size.limits <- c(0, 15)
    rebound.shots.plot.size.breaks <- c(5, 10, 15)
    rebound.shots.plot.size.labels <- c('5', '10', '15+')
    
    # 3
    xg.plot.fill.limits <- c(0.08, 0.18)
    xg.plot.fill.breaks <- c(0.08, 0.18)
    xg.plot.fill.labels <- c('0.08', '0.18')
    
    xg.plot.size.limits <- c(0, 6)
    xg.plot.size.breaks <- c(1, 3, 5)
    xg.plot.size.labels <- c('1', '3', '5+')
    
    # 4
    rebounds.total.plot.fill.limits <- c(0.0005, 0.006)
    rebounds.total.plot.fill.breaks <- c(0.0005, 0.005)
    rebounds.total.plot.fill.labels <- c('0.0005', '0.005')
    
    rebounds.total.plot.size.limits <- c(0, 45)
    rebounds.total.plot.size.breaks <- c(5, 20, 35)
    rebounds.total.plot.size.labels <- c('5', '20', '35+')
    
  } else {
    size.label <- 'Unblocked shot attempts'
    
    # 1
    rebounds.plot.fill.limits <- c(0.35, 0.55)
    rebounds.plot.fill.breaks <- c(0.35, 0.55)
    rebounds.plot.fill.labels <- c('0.35', '0.55')
    
    rebounds.plot.size.limits <- c(0, 45)
    rebounds.plot.size.breaks <- c(5, 20, 35)
    rebounds.plot.size.labels <- c('5', '20', '35+')
    
    # 2
    rebound.shots.plot.fill.limits <- c(0.03, 0.08)
    rebound.shots.plot.fill.breaks <- c(0.03, 0.08)
    rebound.shots.plot.fill.labels <- c('0.03', '0.08')
    
    rebound.shots.plot.size.limits <- c(0, 15)
    rebound.shots.plot.size.breaks <- c(5, 10, 15)
    rebound.shots.plot.size.labels <- c('5', '10', '15+')
    
    # 3
    xg.plot.fill.limits <- c(0.08, 0.18)
    xg.plot.fill.breaks <- c(0.08, 0.18)
    xg.plot.fill.labels <- c('0.08', '0.18')
    
    xg.plot.size.limits <- c(0, 6)
    xg.plot.size.breaks <- c(1, 3, 5)
    xg.plot.size.labels <- c('1', '3', '5+')
    
    # 4
    rebounds.total.plot.fill.limits <- c(0.001, 0.007)
    rebounds.total.plot.fill.breaks <- c(0.001, 0.006)
    rebounds.total.plot.fill.labels <- c('0.001', '0.006')
    
    rebounds.total.plot.size.limits <- c(0, 45)
    rebounds.total.plot.size.breaks <- c(5, 20, 35)
    rebounds.total.plot.size.labels <- c('5', '20', '35+')
  }
  
  # 1
  shots <- get.shots(d)
  shots.hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots.hexbin.df <- data.frame(hexbin::hcell2xy(shots.hexbin),
                                cell = shots.hexbin@cell,
                                count = shots.hexbin@count)
  shots$cell <- shots.hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              rebound.prob.plot = ifelse(unblocked, mean(region.rebound.prob.unblocked), mean(region.rebound.prob))) %>%
    ungroup() %>%
    right_join(shots.hexbin.df, by = 'cell') %>%
    select(cell, x, y, count, shot.count, rebound.prob.plot)
  
  rebounds.plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = rebound.prob.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                        limits = rebounds.plot.fill.limits,
                        breaks = rebounds.plot.fill.breaks,
                        labels = rebounds.plot.fill.labels) +
    scale_size_area(limits = rebounds.plot.size.limits,
                    breaks = rebounds.plot.size.breaks,
                    labels = rebounds.plot.size.labels,
                    oob = squish) +
    labs(title = 'Shots generating rebound',
         subtitle = 'Probability of shot creating rebound',
         caption = 'Created by github.com/j-cqln',
         fill = 'P(rebound)',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  rebounds.hexbin <- hexbin::hexbin(rebounds$x, rebounds$y, xbins = 21, IDs = TRUE)
  rebounds.hexbin.df <- data.frame(hexbin::hcell2xy(rebounds.hexbin),
                                   cell = rebounds.hexbin@cell,
                                   count = rebounds.hexbin@count)
  rebounds$cell <- rebounds.hexbin@cID
  
  rebounds <- rebounds %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              rebound.shot.prob.plot = ifelse(unblocked, mean(region.rebound.shot.prob.unblocked), mean(region.rebound.shot.prob))) %>%
    ungroup() %>%
    right_join(rebounds.hexbin.df, by = 'cell') %>%
    select(cell, x, y, count, shot.count, rebound.shot.prob.plot)
  
  rebound.shots.plot <- rink +
    geom_star(data = rebounds,
              aes(x = x, y = y,
                  fill = rebound.shot.prob.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                        limits = rebound.shots.plot.fill.limits,
                        breaks = rebound.shots.plot.fill.breaks,
                        labels = rebound.shots.plot.fill.labels) +
    scale_size_area(limits = rebound.shots.plot.size.limits,
                    breaks = rebound.shots.plot.size.breaks,
                    labels = rebound.shots.plot.size.labels,
                    oob = squish) +
    labs(title = 'Shots generating rebound shot',
         subtitle = 'Probability of shot creating rebound shot from rebound',
         caption = '',
         fill = 'P(rebound shot | rebound)',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  rebound.shots <- get.shots.generating.rebound.shots(d)
  rebound.shots.hexbin <- hexbin::hexbin(rebound.shots$x, rebound.shots$y, xbins = 21, IDs = TRUE)
  rebound.shots.hexbin.df <- data.frame(hexbin::hcell2xy(rebound.shots.hexbin),
                                        cell = rebound.shots.hexbin@cell,
                                        count = rebound.shots.hexbin@count)
  rebound.shots$cell <- rebound.shots.hexbin@cID
  
  rebound.shots <- rebound.shots %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              xg.plot = ifelse(unblocked, mean(region.rebound.shot.xg.unblocked), mean(region.rebound.shot.xg))) %>%
    ungroup() %>%
    right_join(rebound.shots.hexbin.df, by = 'cell') %>%
    select(cell, x, y, count, shot.count, xg.plot)
  
  xg.plot <- rink +
    geom_star(data = rebound.shots,
              aes(x = x, y = y,
                  fill = xg.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                        limits = xg.plot.fill.limits,
                        breaks = xg.plot.fill.breaks,
                        labels = xg.plot.fill.labels) +
    scale_size_area(limits = xg.plot.size.limits,
                    breaks = xg.plot.size.breaks,
                    labels = xg.plot.size.labels,
                    oob = squish) +
    labs(title = 'Isolated xG of rebound shots',
         subtitle = 'Rebound shot goal probability ignoring last event',
         caption = '',
         fill = 'Isolated rebound shot xG',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  rebounds.total <- get.shots(d)
  rebounds.total.hexbin <- hexbin::hexbin(rebounds.total$x, rebounds.total$y, xbins = 21, IDs = TRUE)
  rebounds.total.hexbin.df <- data.frame(hexbin::hcell2xy(rebounds.total.hexbin),
                                         cell = rebounds.total.hexbin@cell,
                                         count = rebounds.total.hexbin@count)
  rebounds.total$cell <- rebounds.total.hexbin@cID
  
  rebounds.total <- rebounds.total %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              rebounds.total.plot = ifelse(unblocked, mean(region.rebound.total.unblocked), mean(region.rebound.total))) %>%
    ungroup() %>%
    right_join(rebounds.total.hexbin.df, by = 'cell') %>%
    select(cell, x, y, count, shot.count, rebounds.total.plot)
  
  rebounds.total.plot <- rink +
    geom_star(data = rebounds.total,
              aes(x = x, y = y,
                  fill = rebounds.total.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                        limits = rebounds.total.plot.fill.limits,
                        breaks = rebounds.total.plot.fill.breaks,
                        labels = rebounds.total.plot.fill.labels) +
    scale_size_area(limits = rebounds.total.plot.size.limits,
                    breaks = rebounds.total.plot.size.breaks,
                    labels = rebounds.total.plot.size.labels,
                    oob = squish) +
    labs(title = 'Overall \'rebound value\' of shots',
         subtitle = 'Measure of original shot\'s isolated rebound quality',
         caption = '',
         fill = 'Rebound value',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  
  overall.rebounds <- grid.arrange(rebounds.plot,
                                   rebound.shots.plot,
                                   xg.plot,
                                   rebounds.total.plot,
                                   ncol = 4)
  return(overall.rebounds)
}

# Plot xG* and xG comparison
plot.rebounds2 <- function(d, unblocked = FALSE) {
  background.color <- '#ffffff'
  gradient.low.color <- '#bedceb'
  gradient.high.color <- '#004b71'
  diff.gradient.low.color <- '#c8213e'
  diff.gradient.mid.color <- '#d6d6d6'
  diff.gradient.high.color <- '#3597c9'
  
  if (!unblocked) {
    size.label <- 'Shot attempts'
    
    # 1
    xg.no.last.plot.fill.limits <- c(0.013, 0.16)
    xg.no.last.plot.fill.breaks <- c(0.013, 0.16)
    xg.no.last.plot.fill.labels <- c('0.013', '0.16')
    
    xg.no.last.plot.size.limits <- c(0, 45)
    xg.no.last.plot.size.breaks <- c(5, 20, 35)
    xg.no.last.plot.size.labels <- c('5', '20', '35+')
    
    # 2
    rebounds.total.plot.fill.limits <- c(0.0005, 0.006)
    rebounds.total.plot.fill.breaks <- c(0.0005, 0.005)
    rebounds.total.plot.fill.labels <- c('0.0005', '0.005')
    
    rebounds.total.plot.size.limits <- c(0, 45)
    rebounds.total.plot.size.breaks <- c(5, 20, 35)
    rebounds.total.plot.size.labels <- c('5', '20', '35+')
    
    # 3
    sum.plot.fill.limits <- c(0.013, 0.16)
    sum.plot.fill.breaks <- c(0.013, 0.16)
    sum.plot.fill.labels <- c('0.013', '0.16')
    
    sum.plot.size.limits <- c(0, 45)
    sum.plot.size.breaks <- c(5, 20, 35)
    sum.plot.size.labels <- c('5', '20', '35+')
    
    # 4
    diff.plot.fill.limits <- c(-0.006, 0.006)
    diff.plot.fill.breaks <- c(-0.006, 0.006)
    diff.plot.fill.labels <- c('-0.006', '0.006')
    
    diff.plot.size.limits <- c(0, 45)
    diff.plot.size.breaks <- c(5, 20, 35)
    diff.plot.size.labels <- c('5', '20', '35+')
    
  } else {
    size.label <- 'Unblocked shot attempts'
    
    # 1
    xg.no.last.plot.fill.limits <- c(0.018, 0.16)
    xg.no.last.plot.fill.breaks <- c(0.018, 0.16)
    xg.no.last.plot.fill.labels <- c('0.018', '0.16')
    
    xg.no.last.plot.size.limits <- c(0, 45)
    xg.no.last.plot.size.breaks <- c(5, 20, 35)
    xg.no.last.plot.size.labels <- c('5', '20', '35+')
    
    # 2
    rebounds.total.plot.fill.limits <- c(0.001, 0.007)
    rebounds.total.plot.fill.breaks <- c(0.001, 0.007)
    rebounds.total.plot.fill.labels <- c('0.001', '0.007')
    
    rebounds.total.plot.size.limits <- c(0, 45)
    rebounds.total.plot.size.breaks <- c(5, 20, 35)
    rebounds.total.plot.size.labels <- c('5', '20', '35+')
    
    # 3
    sum.plot.fill.limits <- c(0.018, 0.16)
    sum.plot.fill.breaks <- c(0.018, 0.16)
    sum.plot.fill.labels <- c('0.018', '0.16')
    
    sum.plot.size.limits <- c(0, 45)
    sum.plot.size.breaks <- c(5, 20, 35)
    sum.plot.size.labels <- c('5', '20', '35+')
    
    # 4
    diff.plot.fill.limits <- c(-0.006, 0.006)
    diff.plot.fill.breaks <- c(-0.006, 0.006)
    diff.plot.fill.labels <- c('-0.006', '0.006')
    
    diff.plot.size.limits <- c(0, 45)
    diff.plot.size.breaks <- c(5, 20, 35)
    diff.plot.size.labels <- c('5', '20', '35+')
  }
  
  shots <- get.shots(d)
  shots.hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots.hexbin.df <- data.frame(hexbin::hcell2xy(shots.hexbin),
                                cell = shots.hexbin@cell,
                                count = shots.hexbin@count)
  shots$cell <- shots.hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              xg.no.last.plot = ifelse(unblocked,
                                       mean(region.xgoal.unblocked.no.last),
                                       mean(region.xgoal.all.no.last)),
              rebounds.total.plot = ifelse(unblocked,
                                           mean(region.rebound.total.unblocked),
                                           mean(region.rebound.total)),
              sum.plot = ifelse(unblocked,
                                mean(region.xgoal.unblocked.no.last) + mean(region.rebound.total.unblocked),
                                mean(region.xgoal.all.no.last) + mean(region.rebound.total)),
              diff.plot = ifelse(unblocked,
                                 mean(region.xgoal.unblocked.no.last) + mean(region.rebound.total.unblocked) - mean(region.xgoal.unblocked),
                                 mean(region.xgoal.all.no.last) + mean(region.rebound.total) - mean(region.xgoal.all))) %>%
    ungroup() %>%
    right_join(shots.hexbin.df, by = 'cell') %>%
    select(cell, x, y, count, shot.count, xg.no.last.plot, rebounds.total.plot, sum.plot, diff.plot)
  
  # 1
  xg.no.last.plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = xg.no.last.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                        limits = xg.no.last.plot.fill.limits,
                        breaks = xg.no.last.plot.fill.breaks,
                        labels = xg.no.last.plot.fill.labels) +
    scale_size_area(limits = xg.no.last.plot.size.limits,
                    breaks = xg.no.last.plot.size.breaks,
                    labels = xg.no.last.plot.size.labels,
                    oob = squish) +
    labs(title = 'Isolated xG of original shots',
         subtitle = 'Expected goals of original shots, ignoring last event',
         caption = 'Created by github.com/j-cqln',
         fill = 'Isolated shot xG',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  rebounds.total.plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = rebounds.total.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                        limits = rebounds.total.plot.fill.limits,
                        breaks = rebounds.total.plot.fill.breaks,
                        labels = rebounds.total.plot.fill.labels) +
    scale_size_area(limits = rebounds.total.plot.size.limits,
                    breaks = rebounds.total.plot.size.breaks,
                    labels = rebounds.total.plot.size.labels,
                    oob = squish) +
    labs(title = 'Overall \'rebound value\' of shots',
         subtitle = 'Measure of original shot\'s isolated rebound quality',
         caption = '',
         fill = 'Rebound value',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  sum.plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = sum.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient.low.color, high = gradient.high.color, na.value = NA,
                        limits = sum.plot.fill.limits,
                        breaks = sum.plot.fill.breaks,
                        labels = sum.plot.fill.labels) +
    scale_size_area(limits = sum.plot.size.limits,
                    breaks = sum.plot.size.breaks,
                    labels = sum.plot.size.labels,
                    oob = squish) +
    labs(title = 'Isolated xG with rebound value (xG*)',
         subtitle = 'Sum of isolated xG and rebound value (left 2 plots)',
         caption = '',
         fill = 'Isolated xG + rebound value',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  diff.plot <- rink +
    geom_star(data = shots,
              aes(x = x, y = y,
                  fill = diff.plot,
                  size = shot.count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient2(low = diff.gradient.low.color,
                         mid = diff.gradient.mid.color,
                         high = diff.gradient.high.color,
                         na.value = NA,
                         limits = diff.plot.fill.limits,
                         breaks = diff.plot.fill.breaks,
                         labels = diff.plot.fill.labels) +
    scale_size_area(limits = diff.plot.size.limits,
                    breaks = diff.plot.size.breaks,
                    labels = diff.plot.size.labels,
                    oob = squish) +
    labs(title = 'xG* vs xG with last event',
         subtitle = 'Difference between the two models',
         caption = '',
         fill = 'xG* - xG',
         size = size.label) +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
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
  
  xg.rebounds <- grid.arrange(xg.no.last.plot,
                              rebounds.total.plot,
                              sum.plot,
                              diff.plot,
                              ncol = 4)
  
  return(xg.rebounds)
}

# Plot passes
plot.passes <- function(d, players) {
  background.color <- '#ffffff'
  
  d <- d %>% filter(adj.x > 0) %>% select(x, y, danger)
  
  low.danger.passes.plot <- rink +
    geom_density_2d_filled(data = d %>% filter(danger == 'low'),
                           aes(x = x, y = y),
                           alpha = 0.8,
                           show.legend = FALSE) +
    labs(title = 'Passes leading to low danger shot attempts',
         subtitle = 'Lighter indicates higher pass density',
         caption = 'Created by github.com/j-cqln') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color))
  
  high.danger.passes.plot <- rink +
    geom_density_2d_filled(data = d %>% filter(danger == 'high'),
                           aes(x = x, y = y),
                           alpha = 0.8,
                           show.legend = FALSE) +
    labs(title = 'Passes leading to high danger shot attempts',
         subtitle = 'Lighter indicates higher pass density',
         caption = '') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color))
  
  players <- players %>%
    arrange(desc(count)) %>%
    mutate(id = row_number()) %>%
    filter(id < 21) %>%
    arrange(count) %>%
    mutate(player = factor(player, player, ordered = TRUE))
  
  players.plot <- ggplot(data = players,
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
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color))
  
  passes.plots <- grid.arrange(low.danger.passes.plot,
                               high.danger.passes.plot,
                               players.plot,
                               ncol = 3)
  
  return(passes.plots)
}

# Plot zone entries
plot.ozone.entries <- function(d, players) {
  background.color <- '#ffffff'
  
  ozone.entries.plot <- rink +
    geom_density_2d_filled(data = d,
                           aes(x = x, y = y),
                           alpha = 0.9,
                           show.legend = FALSE) +
    labs(title = 'Successful offensive zone entries',
         subtitle = 'Lighter indicates higher success density, bottom is o-zone',
         caption = 'Created by github.com/j-cqln',
         x = 'East-west direction of rink (along blue line)', 
         y = 'North-south direction of rink') +
    coord_cartesian(ylim = c(-30, -20),
                    xlim = c(-42.5, 42.5)) +
    theme_pub(base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color),
          legend.background = element_rect(fill = background.color),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  players <- players %>%
    top_n(count.per.60, n = 20) %>%
    arrange(count.per.60)
  
  players <- players %>%
    distinct(player, .keep_all = TRUE)
  
  players$player = factor(players$player, players$player, ordered = TRUE)
  
  players.plot <- ggplot(data = players,
                         aes(x = count.per.60, y = player)) +
    geom_segment(aes(x = 6, xend = count.per.60,
                     y = player, yend = player,
                     color = position),
                 show.legend = FALSE) +
    geom_point(aes(color = position),
               show.legend = FALSE) +
    scale_color_manual(breaks = c('F', 'D'),
                       values = c('F' = pubred, 'D' = pubblue)) +
    geom_text(aes(label = round(count.per.60, 1)), hjust = -0.3) +
    labs(title = 'Top 20 players by successful offensive zone entries/60',
         subtitle = 'Red indicates forwards, blue indicates defense',
         # caption  = '',
         x = 'Successful offensive zone entries per 60 minutes played',
         y = NULL) +
    xlim(c(6, 26)) +
    theme_pub(type = 'pop', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background.color),
          panel.background = element_rect(fill = background.color))
  
  ozone.entries.plots <- grid.arrange(ozone.entries.plot,
                                      players.plot,
                                      ncol = 2)
  
  return(ozone.entries.plots)
}