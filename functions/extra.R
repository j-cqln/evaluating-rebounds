# NHL data for comparison

source('functions/modeling.R')
source('functions/prepare.R')
source('functions/rink.R')
source('functions/visualize.R')

# Functions that need to be replaced
# Data cleaning, etc.
process.data <- function(d) {
  # Shot info
  d <- d %>%
    rename(event_id = id,
           shot_id = shotID,
           season_type = isPlayoffGame,
           game = game_id,
           home_team_code = homeTeamCode,
           away_team_code = awayTeamCode,
           home_team_skaters_on_ice = homeSkatersOnIce,
           away_team_skaters_on_ice = awaySkatersOnIce,
           adj_x = xCordAdjusted,
           adj_y = yCordAdjusted,
           dist = shotDistance,
           abs_angle = shotAngleAdjusted,
           angle_change = shotAnglePlusRebound,
           last_event = lastEventCategory,
           time_since_last = timeSinceLastEvent,
           last_event_team = lastEventTeam,
           shooter = shooterName,
           goalie = goalieNameForShot,
           on_goal = shotWasOnGoal,
           creates_rebound_shot = shotGeneratedRebound,
           frozen = shotGoalieFroze,
           stopped = shotPlayStopped,
           in_zone = shotPlayContinuedInZone,
           out_zone = shotPlayContinuedOutsideZone,
           shot_type = shotType) %>%
    filter(!is.na(shooter)) %>%
    mutate(id = row_number(),
           team = ifelse(team == 'HOME', home_team_code, away_team_code),
           is_home_team = ifelse(team == 'HOME', 1, 0),
           goal_diff = ifelse(is_home_team == 1,
                              homeTeamGoals - awayTeamGoals,
                              awayTeamGoals - homeTeamGoals),
           event = 'shot',
           goalie = ifelse(!is.na(goalie), goalie, ''),
           rebound_shot = creates_rebound_shot,
           play_stopped = ifelse((frozen == 1) | (stopped == 1), 1, 0),
           time_since_last = ifelse(time_since_last <= last_event_time_cutoff,
                                    time_since_last,
                                    time_since_last_default),
           last_event = ifelse(time_since_last <= last_event_time_cutoff,
                               last_event,
                               'none'),
           last_event = ifelse(is.na(last_event), 'none', last_event),
           last_event_team = ifelse(last_event == 'none', NA, last_event_team))
  
  x <- d$adj_x
  y <- d$adj_y
  
  x <- -abs(x)
  
  d$x <- -y
  d$y <- x
  
  return(d)
}

# Additional data processing for model use
process.model.data <- function(d) {
  d$goal_diff <- as.factor(d$goal_diff)
  d$shooter <- as.factor(d$shooter)
  d$goalie <- as.factor(d$goalie)
  d$shot_type <- as.factor(d$shot_type)
  d$last_event <- as.factor(d$last_event)
  
  d <- d %>%
    select(id, season, season_type, game,
           team, period,
           time, event,
           dist, abs_angle,
           goal_diff, time_since_last, last_event,
           shooter, goalie, shot_type,
           on_goal, goal, rebound_shot, play_stopped, in_zone, out_zone)
  
  return(d)
}

# Find region-specific information for rebounds
get.regions.rebounds <- function(d, regions) {
  d2 <- d %>%
    mutate(rebound_shot_xg = ifelse(creates_rebound_shot == 1,
                                    lead(xgoal_unblocked_no_last), NA))
  d2 <- d2 %>% select(id, rebound_shot_xg)
  d <- left_join(d, d2, by = 'id')
  
  d <- d %>%
    mutate(rebound_shot_xg = ifelse(event == 'shot', rebound_shot_xg, NA))
  
  d$region_rebound_shot_prob_unblocked <- NA
  d$region_rebound_shot_xg_unblocked <- NA
  d$region_shot_count_unblocked <- NA
  d$region_xgoal_unblocked <- NA
  d$region_xgoal_unblocked_no_last <- NA
  
  for (region_name in region_names) {
    # Unblocked shot attempts
    region_rebound_shot_prob_unblocked <- sum(d$creates_rebound_shot[d$region == region_name]) / nrow(d[d$region == region_name, ])
    region_rebound_shot_xg_unblocked <- mean(d$rebound_shot_xg[d$region == region_name], na.rm = TRUE)
    region_xgoal_unblocked <- mean(d$xgoal_unblocked[d$region == region_name], na.rm = TRUE)
    region_xgoal_unblocked_no_last <- mean(d$xgoal_unblocked_no_last[d$region == region_name], na.rm = TRUE)
    
    regions$region_rebound_shot_prob_unblocked[regions$region == region_name] <- region_rebound_shot_prob_unblocked
    regions$region_rebound_shot_xg_unblocked[regions$region == region_name] <- region_rebound_shot_xg_unblocked
    regions$region_xgoal_unblocked[regions$region == region_name] <- region_xgoal_unblocked
    regions$region_xgoal_unblocked_no_last[regions$region == region_name] <- region_xgoal_unblocked_no_last
    
    d$region_rebound_shot_prob_unblocked[d$region == region_name] <- region_rebound_shot_prob_unblocked
    d$region_rebound_shot_xg_unblocked[d$region == region_name] <- region_rebound_shot_xg_unblocked
    d$region_xgoal_unblocked[d$region == region_name] <- region_xgoal_unblocked
    d$region_xgoal_unblocked_no_last[d$region == region_name] <- region_xgoal_unblocked_no_last
    
    region_shot_count_unblocked <- nrow(d[d$region == region_name, ])
    d$region_shot_count_unblocked[d$region == region_name] <- region_shot_count_unblocked
    regions$region_shot_count_unblocked[regions$region == region_name] <- region_shot_count_unblocked
  }
  
  regions$region_rebound_total_unblocked <- regions$region_rebound_shot_prob_unblocked * regions$region_rebound_shot_xg_unblocked
  d$region_rebound_total_unblocked <- d$region_rebound_shot_prob_unblocked * d$region_rebound_shot_xg_unblocked
  
  return(list(d = d, regions = regions))
}

# Plot rink regions with corresponding shot counts
plot.regions <- function(d) {
  region_outline_color <- '#000000'
  background_color <- '#ffffff'
  gradient_low_color <- '#b0f0ce'
  gradient_high_color <- '#055229'
  
  fill_label <- 'Unblocked shot attempts'
  
  regions_fill_limits <- c(6400, 27100)
  regions_fill_breaks <- c(6500, 27000)
  regions_fill_labels <- c('6500', '27000')
  
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
  
  return(regions_plot)
}

# Plot average xG by region
plot.xg <- function(d) {
  region_outline_color <- '#000000'
  background_color <- '#ffffff'
  gradient_low_color <- '#b0f0ce'
  gradient_high_color <- '#055229'
  
  fill_label <- 'xG'
  
  regions_fill_limits <- c(0.02, 0.15)
  regions_fill_breaks <- c(0.02, 0.14)
  regions_fill_labels <- c('0.02', '0.14')
  
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
    labs(title = 'xG, NHL unblocked shots',
         subtitle = 'Expected goals, model trained on unblocked shots',
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
  
  return(xg_plot)
}

# Plot rebound value
plot.rebounds1 <- function(d) {
  background_color <- '#ffffff'
  gradient_low_color <- '#bedceb'
  gradient_high_color <- '#004b71'
  
  size_label <- 'Unblocked shot attempts'
  
  # 1/2
  rebounds_plot_fill_limits <- c(0.04, 0.11)
  rebounds_plot_fill_breaks <- c(0.05, 0.10)
  rebounds_plot_fill_labels <- c('0.05', '0.10')
  
  rebounds_plot_size_limits <- c(0, 600)
  rebounds_plot_size_breaks <- c(200, 400, 600)
  rebounds_plot_size_labels <- c('200', '400', '600+')
  
  # 3
  xg_plot_fill_limits <- c(0.11, 0.155)
  xg_plot_fill_breaks <- c(0.11, 0.15)
  xg_plot_fill_labels <- c('0.11', '0.15')
  
  xg_plot_size_limits <- c(0, 120)
  xg_plot_size_breaks <- c(40, 80, 120)
  xg_plot_size_labels <- c('40', '80', '120+')
  
  # 4
  rebounds_total_plot_fill_limits <- c(0.005, 0.0165)
  rebounds_total_plot_fill_breaks <- c(0.005, 0.016)
  rebounds_total_plot_fill_labels <- c('0.005', '0.016')
  
  rebounds_total_plot_size_limits <- c(0, 600)
  rebounds_total_plot_size_breaks <- c(200, 400, 600)
  rebounds_total_plot_size_labels <- c('200', '400', '600+')
  
  # 1/2
  shots <- get.shots(d)
  shots_hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots_hexbin_df <- data.frame(hexbin::hcell2xy(shots_hexbin),
                                cell = shots_hexbin@cell,
                                count = shots_hexbin@count)
  shots$cell <- shots_hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              rebound_prob_plot = mean(region_rebound_shot_prob_unblocked)) %>%
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
    labs(title = 'Shots generating rebound shots',
         subtitle = 'Probability of shot creating rebound shots',
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
  
  # 3
  rebound_shots <- d[d$rebound_shot == 1, ]
  rebound_shots_hexbin <- hexbin::hexbin(rebound_shots$x, rebound_shots$y, xbins = 21, IDs = TRUE)
  rebound_shots_hexbin_df <- data.frame(hexbin::hcell2xy(rebound_shots_hexbin),
                                        cell = rebound_shots_hexbin@cell,
                                        count = rebound_shots_hexbin@count)
  rebound_shots$cell <- rebound_shots_hexbin@cID
  
  rebound_shots <- rebound_shots %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              xg_plot = mean(region_rebound_shot_xg_unblocked)) %>%
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
              rebounds_total_plot = mean(region_rebound_total_unblocked)) %>%
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
                                   xg_plot,
                                   rebounds_total_plot,
                                   ncol = 3)
  return(overall_rebounds)
}

# Plot xG* and xG comparison
plot.rebounds2 <- function(d) {
  background_color <- '#ffffff'
  gradient_low_color <- '#bedceb'
  gradient_high_color <- '#004b71'
  diff_gradient_low_color <- '#c8213e'
  diff_gradient_mid_color <- '#d6d6d6'
  diff_gradient_high_color <- '#3597c9'
  
  size_label <- 'Unblocked shot attempts'
  
  # 1
  xg_no_last_plot_fill_limits <- c(0.02, 0.17)
  xg_no_last_plot_fill_breaks <- c(0.02, 0.17)
  xg_no_last_plot_fill_labels <- c('0.02', '0.17')
  
  xg_no_last_plot_size_limits <- c(0, 600)
  xg_no_last_plot_size_breaks <- c(200, 400, 600)
  xg_no_last_plot_size_labels <- c('200', '400', '600+')
  
  # 2
  rebounds_total_plot_fill_limits <- c(0.005, 0.017)
  rebounds_total_plot_fill_breaks <- c(0.005, 0.017)
  rebounds_total_plot_fill_labels <- c('0.005', '0.017')
  
  rebounds_total_plot_size_limits <- c(0, 600)
  rebounds_total_plot_size_breaks <- c(200, 400, 600)
  rebounds_total_plot_size_labels <- c('200', '400', '600+')
  
  # 3
  sum_plot_fill_limits <- c(0.02, 0.17)
  sum_plot_fill_breaks <- c(0.02, 0.17)
  sum_plot_fill_labels <- c('0.02', '0.17')
  
  sum_plot_size_limits <- c(0, 600)
  sum_plot_size_breaks <- c(200, 400, 600)
  sum_plot_size_labels <- c('200', '400', '600+')
  
  # 4
  diff_plot_fill_limits <- c(0, 0.02)
  diff_plot_fill_breaks <- c(0, 0.02)
  diff_plot_fill_labels <- c('0', '0.02')
  
  diff_plot_size_limits <- c(0, 600)
  diff_plot_size_breaks <- c(200, 400, 600)
  diff_plot_size_labels <- c('200', '400', '600+')
  
  shots <- get.shots(d)
  shots_hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots_hexbin_df <- data.frame(hexbin::hcell2xy(shots_hexbin),
                                cell = shots_hexbin@cell,
                                count = shots_hexbin@count)
  shots$cell <- shots_hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              xg_no_last_plot = mean(region_xgoal_unblocked_no_last),
              rebounds_total_plot = mean(region_rebound_total_unblocked),
              sum_plot = mean(region_xgoal_unblocked_no_last) + mean(region_rebound_total_unblocked),
              diff_plot = mean(region_xgoal_unblocked_no_last) + mean(region_rebound_total_unblocked) - mean(region_xgoal_unblocked)) %>%
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

# Read data
files_available = TRUE

file_names = c('data/shots_nhl.rds',
               'data/shots_unblocked_nhl.rds',
               'data/regions_unblocked_nhl.rds',
               'data/models_nhl.rds',
               'data/metrics_nhl.rds')

if (all(file.exists(file_names))) {
  shots <- readRDS('data/shots_nhl.rds')
  
  shots_unblocked <- readRDS('data/shots_unblocked_nhl.rds')
  regions_unblocked <- readRDS('data/regions_unblocked_nhl.rds')
  
  models <- readRDS('data/models_nhl.rds')
  metrics <- readRDS('data/metrics_nhl.rds')
  
} else {
  shots <- read_csv('data/DATA_FILE_NAME.csv')
  shots <- process.data(shots)
  
  # xG for use with rebounds
  unblocked_shots <- process.model.data(shots)
  
  model_goal_unblocked <- model(unblocked_shots, unblocked_shots$goal)
  unblocked_shots$xgoal_unblocked <- predict(model_goal_unblocked, unblocked_shots, allow.new.levels = TRUE, type = 'response')
  a_goal_unblocked <- auc(as.integer(as.logical(unblocked_shots$goal)), unblocked_shots$xgoal_unblocked)
  ll_goal_unblocked <- logLoss(unblocked_shots$goal, unblocked_shots$xgoal_unblocked)
  
  unblocked_shots$xgoal_unblocked_no_last <- predict(model_goal_unblocked,
                                                     unblocked_shots %>% mutate(last_event = 'none'),
                                                     allow.new.levels = TRUE,
                                                     type = 'response')
  a_goal_unblocked_no_last <- auc(as.integer(as.logical(unblocked_shots$goal)), unblocked_shots$xgoal_unblocked_no_last)
  ll_goal_unblocked_no_last <- logLoss(unblocked_shots$goal, unblocked_shots$xgoal_unblocked_no_last)
  
  models <- list(model_goal_unblocked)
  metrics <- data.frame(model = c('goal_unblocked', 'goal_unblocked_no_last'),
                        train.auc = c(a_goal_unblocked, a_goal_unblocked_no_last),
                        train.log.loss = c(ll_goal_unblocked, ll_goal_unblocked_no_last))
  
  # Rebounds
  shots <- left_join(shots, unblocked_shots %>% select(id, xgoal_unblocked, xgoal_unblocked_no_last), by = 'id')
  
  retval <- get.regions(shots)
  retval <- get.regions.rebounds(retval$d, retval$regions)
  shots_unblocked <- retval$d
  regions_unblocked <- retval$regions
  
  rm(retval)
  
  # Save
  saveRDS(shots, 'data/shots_nhl.rds')
  
  saveRDS(shots_unblocked, 'data/shots_unblocked_nhl.rds')
  saveRDS(regions_unblocked, 'data/regions_unblocked_nhl.rds')
  
  saveRDS(models, 'data/models_nhl.rds')
  saveRDS(metrics, 'data/metrics_nhl.rds')
}

# Regions
regions_plot_unblocked <- plot.regions(regions_unblocked)

ggsave(filename = paste0('www/', 'regions_unblocked_nhl', '.jpg'),
       plot = regions_plot_unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# xG
xg_plot_unblocked <- plot.xg(regions_unblocked)

ggsave(filename = paste0('www/', 'xg_unblocked_nhl', '.jpg'),
       plot = xg_plot_unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# Rebounds
overall_rebounds_unblocked <- plot.rebounds1(shots_unblocked)

ggsave(filename = paste0('www/', 'rebounds_unblocked_nhl', '.jpg'),
       plot = overall_rebounds_unblocked,
       width = 15,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg_rebounds_unblocked <- plot.rebounds2(shots_unblocked)

ggsave(filename = paste0('www/', 'xg_rebounds_unblocked_nhl', '.jpg'),
       plot = xg_rebounds_unblocked,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)