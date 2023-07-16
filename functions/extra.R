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
    rename(event.id = id,
           shot.id = shotID,
           season.type = isPlayoffGame,
           game = game_id,
           home.team.code = homeTeamCode,
           away.team.code = awayTeamCode,
           home.team.skaters.on.ice = homeSkatersOnIce,
           away.team.skaters.on.ice = awaySkatersOnIce,
           adj.x = xCordAdjusted,
           adj.y = yCordAdjusted,
           dist = shotDistance,
           abs.angle = shotAngleAdjusted,
           angle.change = shotAnglePlusRebound,
           last.event = lastEventCategory,
           time.since.last = timeSinceLastEvent,
           last.event.team = lastEventTeam,
           shooter = shooterName,
           goalie = goalieNameForShot,
           on.goal = shotWasOnGoal,
           creates.rebound.shot = shotGeneratedRebound,
           frozen = shotGoalieFroze,
           stopped = shotPlayStopped,
           in.zone = shotPlayContinuedInZone,
           out.zone = shotPlayContinuedOutsideZone,
           shot.type = shotType) %>%
    filter(!is.na(shooter)) %>%
    mutate(id = row_number(),
           team = ifelse(team == 'HOME', home.team.code, away.team.code),
           is.home.team = ifelse(team == 'HOME', 1, 0),
           goal.diff = ifelse(is.home.team == 1,
                              homeTeamGoals - awayTeamGoals,
                              awayTeamGoals - homeTeamGoals),
           event = 'shot',
           goalie = ifelse(!is.na(goalie), goalie, ''),
           rebound.shot = creates.rebound.shot,
           play.stopped = ifelse((frozen == 1) | (stopped == 1), 1, 0),
           time.since.last = ifelse(time.since.last <= last.event.time.cutoff,
                                    time.since.last,
                                    time.since.last.default),
           last.event = ifelse(time.since.last <= last.event.time.cutoff,
                               last.event,
                               'none'),
           last.event = ifelse(is.na(last.event), 'none', last.event),
           last.event.team = ifelse(last.event == 'none', NA, last.event.team))
  
  x <- d$adj.x
  y <- d$adj.y
  
  x <- -abs(x)
  
  d$x <- -y
  d$y <- x
  
  return(d)
}

# Additional data processing for model use
process.model.data <- function(d) {
  d$goal.diff <- as.factor(d$goal.diff)
  d$shooter <- as.factor(d$shooter)
  d$goalie <- as.factor(d$goalie)
  d$shot.type <- as.factor(d$shot.type)
  d$last.event <- as.factor(d$last.event)
  
  d <- d %>%
    select(id, season, season.type, game,
           team, period,
           time, event,
           dist, abs.angle,
           goal.diff, time.since.last, last.event,
           shooter, goalie, shot.type,
           on.goal, goal, rebound.shot, play.stopped, in.zone, out.zone)
  
  return(d)
}

# Find region-specific information for rebounds
get.regions.rebounds <- function(d, regions) {
  d2 <- d %>%
    mutate(rebound.shot.xg = ifelse(creates.rebound.shot == 1,
                                    lead(xgoal.unblocked.no.last), NA))
  d2 <- d2 %>% select(id, rebound.shot.xg)
  d <- left_join(d, d2, by = 'id')
  
  d <- d %>%
    mutate(rebound.shot.xg = ifelse(event == 'shot', rebound.shot.xg, NA))
  
  d$region.rebound.shot.prob.unblocked <- NA
  d$region.rebound.shot.xg.unblocked <- NA
  d$region.shot.count.unblocked <- NA
  d$region.xgoal.unblocked <- NA
  d$region.xgoal.unblocked.no.last <- NA
  
  for (region.name in region.names) {
    # Unblocked shot attempts
    region.rebound.shot.prob.unblocked <- sum(d$creates.rebound.shot[d$region == region.name]) / nrow(d[d$region == region.name, ])
    region.rebound.shot.xg.unblocked <- mean(d$rebound.shot.xg[d$region == region.name], na.rm = TRUE)
    region.xgoal.unblocked <- mean(d$xgoal.unblocked[d$region == region.name], na.rm = TRUE)
    region.xgoal.unblocked.no.last <- mean(d$xgoal.unblocked.no.last[d$region == region.name], na.rm = TRUE)
    
    regions$region.rebound.shot.prob.unblocked[regions$region == region.name] <- region.rebound.shot.prob.unblocked
    regions$region.rebound.shot.xg.unblocked[regions$region == region.name] <- region.rebound.shot.xg.unblocked
    regions$region.xgoal.unblocked[regions$region == region.name] <- region.xgoal.unblocked
    regions$region.xgoal.unblocked.no.last[regions$region == region.name] <- region.xgoal.unblocked.no.last
    
    d$region.rebound.shot.prob.unblocked[d$region == region.name] <- region.rebound.shot.prob.unblocked
    d$region.rebound.shot.xg.unblocked[d$region == region.name] <- region.rebound.shot.xg.unblocked
    d$region.xgoal.unblocked[d$region == region.name] <- region.xgoal.unblocked
    d$region.xgoal.unblocked.no.last[d$region == region.name] <- region.xgoal.unblocked.no.last
    
    region.shot.count.unblocked <- nrow(d[d$region == region.name, ])
    d$region.shot.count.unblocked[d$region == region.name] <- region.shot.count.unblocked
    regions$region.shot.count.unblocked[regions$region == region.name] <- region.shot.count.unblocked
  }
  
  regions$region.rebound.total.unblocked <- regions$region.rebound.shot.prob.unblocked * regions$region.rebound.shot.xg.unblocked
  d$region.rebound.total.unblocked <- d$region.rebound.shot.prob.unblocked * d$region.rebound.shot.xg.unblocked
  
  return(list(d = d, regions = regions))
}

# Plot rink regions with corresponding shot counts
plot.regions <- function(d) {
  region.outline.color <- '#000000'
  background.color <- '#ffffff'
  gradient.low.color <- '#b0f0ce'
  gradient.high.color <- '#055229'
  
  fill.label <- 'Unblocked shot attempts'
  
  regions.fill.limits <- c(6400, 27100)
  regions.fill.breaks <- c(6500, 27000)
  regions.fill.labels <- c('6500', '27000')
  
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
  
  return(regions.plot)
}

# Plot average xG by region
plot.xg <- function(d) {
  region.outline.color <- '#000000'
  background.color <- '#ffffff'
  gradient.low.color <- '#b0f0ce'
  gradient.high.color <- '#055229'
  
  fill.label <- 'xG'
  
  regions.fill.limits <- c(0.02, 0.15)
  regions.fill.breaks <- c(0.02, 0.14)
  regions.fill.labels <- c('0.02', '0.14')
  
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
    labs(title = 'xG, NHL unblocked shots',
         subtitle = 'Expected goals, model trained on unblocked shots',
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
  
  return(xg.plot)
}

# Plot rebound value
plot.rebounds1 <- function(d) {
  background.color <- '#ffffff'
  gradient.low.color <- '#bedceb'
  gradient.high.color <- '#004b71'
  
  size.label <- 'Unblocked shot attempts'
  
  # 1/2
  rebounds.plot.fill.limits <- c(0.04, 0.11)
  rebounds.plot.fill.breaks <- c(0.05, 0.10)
  rebounds.plot.fill.labels <- c('0.05', '0.10')
  
  rebounds.plot.size.limits <- c(0, 600)
  rebounds.plot.size.breaks <- c(200, 400, 600)
  rebounds.plot.size.labels <- c('200', '400', '600+')
  
  # 3
  xg.plot.fill.limits <- c(0.11, 0.155)
  xg.plot.fill.breaks <- c(0.11, 0.15)
  xg.plot.fill.labels <- c('0.11', '0.15')
  
  xg.plot.size.limits <- c(0, 120)
  xg.plot.size.breaks <- c(40, 80, 120)
  xg.plot.size.labels <- c('40', '80', '120+')
  
  # 4
  rebounds.total.plot.fill.limits <- c(0.005, 0.0165)
  rebounds.total.plot.fill.breaks <- c(0.005, 0.016)
  rebounds.total.plot.fill.labels <- c('0.005', '0.016')
  
  rebounds.total.plot.size.limits <- c(0, 600)
  rebounds.total.plot.size.breaks <- c(200, 400, 600)
  rebounds.total.plot.size.labels <- c('200', '400', '600+')
  
  # 1/2
  shots <- get.shots(d)
  shots.hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots.hexbin.df <- data.frame(hexbin::hcell2xy(shots.hexbin),
                                cell = shots.hexbin@cell,
                                count = shots.hexbin@count)
  shots$cell <- shots.hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              rebound.prob.plot = mean(region.rebound.shot.prob.unblocked)) %>%
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
    labs(title = 'Shots generating rebound shots',
         subtitle = 'Probability of shot creating rebound shots',
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
  
  # 3
  rebound.shots <- d[d$rebound.shot == 1, ]
  rebound.shots.hexbin <- hexbin::hexbin(rebound.shots$x, rebound.shots$y, xbins = 21, IDs = TRUE)
  rebound.shots.hexbin.df <- data.frame(hexbin::hcell2xy(rebound.shots.hexbin),
                                        cell = rebound.shots.hexbin@cell,
                                        count = rebound.shots.hexbin@count)
  rebound.shots$cell <- rebound.shots.hexbin@cID
  
  rebound.shots <- rebound.shots %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              xg.plot = mean(region.rebound.shot.xg.unblocked)) %>%
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
              rebounds.total.plot = mean(region.rebound.total.unblocked)) %>%
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
                                   xg.plot,
                                   rebounds.total.plot,
                                   ncol = 3)
  return(overall.rebounds)
}

# Plot xG* and xG comparison
plot.rebounds2 <- function(d) {
  background.color <- '#ffffff'
  gradient.low.color <- '#bedceb'
  gradient.high.color <- '#004b71'
  diff.gradient.low.color <- '#c8213e'
  diff.gradient.mid.color <- '#d6d6d6'
  diff.gradient.high.color <- '#3597c9'
  
  size.label <- 'Unblocked shot attempts'
  
  # 1
  xg.no.last.plot.fill.limits <- c(0.02, 0.17)
  xg.no.last.plot.fill.breaks <- c(0.02, 0.17)
  xg.no.last.plot.fill.labels <- c('0.02', '0.17')
  
  xg.no.last.plot.size.limits <- c(0, 600)
  xg.no.last.plot.size.breaks <- c(200, 400, 600)
  xg.no.last.plot.size.labels <- c('200', '400', '600+')
  
  # 2
  rebounds.total.plot.fill.limits <- c(0.005, 0.017)
  rebounds.total.plot.fill.breaks <- c(0.005, 0.017)
  rebounds.total.plot.fill.labels <- c('0.005', '0.017')
  
  rebounds.total.plot.size.limits <- c(0, 600)
  rebounds.total.plot.size.breaks <- c(200, 400, 600)
  rebounds.total.plot.size.labels <- c('200', '400', '600+')
  
  # 3
  sum.plot.fill.limits <- c(0.02, 0.17)
  sum.plot.fill.breaks <- c(0.02, 0.17)
  sum.plot.fill.labels <- c('0.02', '0.17')
  
  sum.plot.size.limits <- c(0, 600)
  sum.plot.size.breaks <- c(200, 400, 600)
  sum.plot.size.labels <- c('200', '400', '600+')
  
  # 4
  diff.plot.fill.limits <- c(0, 0.02)
  diff.plot.fill.breaks <- c(0, 0.02)
  diff.plot.fill.labels <- c('0', '0.02')
  
  diff.plot.size.limits <- c(0, 600)
  diff.plot.size.breaks <- c(200, 400, 600)
  diff.plot.size.labels <- c('200', '400', '600+')
  
  shots <- get.shots(d)
  shots.hexbin <- hexbin::hexbin(shots$x, shots$y, xbins = 21, IDs = TRUE)
  shots.hexbin.df <- data.frame(hexbin::hcell2xy(shots.hexbin),
                                cell = shots.hexbin@cell,
                                count = shots.hexbin@count)
  shots$cell <- shots.hexbin@cID
  
  shots <- shots %>%
    group_by(cell) %>%
    summarise(shot.count = n(),
              xg.no.last.plot = mean(region.xgoal.unblocked.no.last),
              rebounds.total.plot = mean(region.rebound.total.unblocked),
              sum.plot = mean(region.xgoal.unblocked.no.last) + mean(region.rebound.total.unblocked),
              diff.plot = mean(region.xgoal.unblocked.no.last) + mean(region.rebound.total.unblocked) - mean(region.xgoal.unblocked)) %>%
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

# Read data
files.available = TRUE

file.names = c('data/shots_nhl.rds',
               'data/shots_unblocked_nhl.rds',
               'data/regions_unblocked_nhl.rds',
               'data/models_nhl.rds',
               'data/metrics_nhl.rds')

if (all(file.exists(file.names))) {
  shots <- readRDS('data/shots_nhl.rds')
  
  shots_unblocked <- readRDS('data/shots_unblocked_nhl.rds')
  regions_unblocked <- readRDS('data/regions_unblocked_nhl.rds')
  
  models <- readRDS('data/models_nhl.rds')
  metrics <- readRDS('data/metrics_nhl.rds')
  
} else {
  shots <- read_csv('data/DATA_FILE_NAME.csv')
  shots <- process.data(shots)
  
  # xG for use with rebounds
  unblocked.shots <- process.model.data(shots)
  
  model.goal.unblocked <- model(unblocked.shots, unblocked.shots$goal)
  unblocked.shots$xgoal.unblocked <- predict(model.goal.unblocked, unblocked.shots, allow.new.levels = TRUE, type = 'response')
  a.goal.unblocked <- auc(as.integer(as.logical(unblocked.shots$goal)), unblocked.shots$xgoal.unblocked)
  ll.goal.unblocked <- logLoss(unblocked.shots$goal, unblocked.shots$xgoal.unblocked)
  
  unblocked.shots$xgoal.unblocked.no.last <- predict(model.goal.unblocked,
                                                     unblocked.shots %>% mutate(last.event = 'none'),
                                                     allow.new.levels = TRUE,
                                                     type = 'response')
  a.goal.unblocked.no.last <- auc(as.integer(as.logical(unblocked.shots$goal)), unblocked.shots$xgoal.unblocked.no.last)
  ll.goal.unblocked.no.last <- logLoss(unblocked.shots$goal, unblocked.shots$xgoal.unblocked.no.last)
  
  models <- list(model.goal.unblocked)
  metrics <- data.frame(model = c('goal.unblocked', 'goal.unblocked.no.last'),
                        train.auc = c(a.goal.unblocked, a.goal.unblocked.no.last),
                        train.log.loss = c(ll.goal.unblocked, ll.goal.unblocked.no.last))
  
  # Rebounds
  shots <- left_join(shots, unblocked.shots %>% select(id, xgoal.unblocked, xgoal.unblocked.no.last), by = 'id')
  
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
regions.plot.unblocked <- plot.regions(regions_unblocked)

ggsave(filename = paste0('www/', 'regions_unblocked_nhl', '.jpg'),
       plot = regions.plot.unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# xG
xg.plot.unblocked <- plot.xg(regions_unblocked)

ggsave(filename = paste0('www/', 'xg_unblocked_nhl', '.jpg'),
       plot = xg.plot.unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# Rebounds
overall.rebounds.unblocked <- plot.rebounds1(shots_unblocked)

ggsave(filename = paste0('www/', 'rebounds_unblocked_nhl', '.jpg'),
       plot = overall.rebounds.unblocked,
       width = 15,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg.rebounds.unblocked <- plot.rebounds2(shots_unblocked)

ggsave(filename = paste0('www/', 'xg_rebounds_unblocked_nhl', '.jpg'),
       plot = xg.rebounds.unblocked,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)