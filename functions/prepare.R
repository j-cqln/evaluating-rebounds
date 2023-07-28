library(dplyr)
library(sp)

# Time cutoffs and defaults in seconds
rebound_time_cutoff <- 3
last_event_time_cutoff <- 5
time_since_last_default <- 99

# Regions used
region_names <- c('inner.slot', 'right.slot', 'left.slot', 'high.slot',
                  'right.outside', 'left.outside',
                  'right.point', 'left.point', 'center.point',
                  'other')

# Rename data columns
process.names <- function(d) {
  d <- d %>%
    rename(season_type = seasonstage,
           team = teamname,
           opposing_team = opposingteamname,
           is_home_team = ishomegame,
           possession_team = teaminpossession,
           current_possession = currentpossession,
           strength_state = strengthstate,
           team_skaters_on_ice = teamskatersonicecount,
           opposing_team_skaters_on_ice = opposingteamskatersonicecount,
           time = compiledgametime,
           goal_diff = scoredifferential,
           adj_x = xadjcoord,
           adj_y = yadjcoord,
           shooter = player,
           shooter_position = playerprimaryposition,
           own_goalie = goalie,
           goalie = opposing_goalie,
           event = eventname,
           creates_rebound = createsrebound,
           shot_type = shottype,
           shot_aim = shotaim) %>%
    mutate(season = 2023)
  
  return(d)
}

# Convert coordinates into plot-able format
process.coords <- function(d, half_rink = TRUE, horizontal = FALSE) {
  x <- d$adj_x
  y <- d$adj_y
  
  if (half_rink == TRUE) {
    x <- -abs(x)
  }
  
  if (horizontal == TRUE) {
    d$x <- x
    d$y <- ifelse(d$adj_x > 0, -y, y)
  } else {
    d$x <- ifelse(d$adj_x > 0, y, -y)
    d$y <- x
  }
  
  return(d)
}

# Data cleaning, etc.
process.data <- function(d) {
  d <- process.names(d)
  d <- process.coords(d, half_rink = TRUE, horizontal = FALSE)
  
  # Shot info
  d <- d %>%
    mutate(id = row_number(),
           own_goalie = ifelse(!is.na(own_goalie), own_goalie, ''),
           goalie = ifelse(!is.na(goalie), goalie, ''),
           dist = sqrt(x^2 + (y + 89)^2),
           angle = atan(x / (y + 89)) * (180 / pi),
           abs_angle = abs(angle),
           time_since_last = ifelse((time - lag(time)) >= 0 &
                                      date == lag(date) &
                                      game == lag(game) &
                                      period == lag(period),
                                    time - lag(time),
                                    time_since_last_default),
           time_since_last = ifelse(!is.na(time_since_last),
                                    time_since_last,
                                    time_since_last_default),
           last_event = ifelse(time_since_last <= last_event_time_cutoff,
                               lag(event),
                               'none'),
           last_event = ifelse(is.na(last_event), 'none', last_event),
           last_event_team = ifelse(last_event == 'none', NA, lag(team)),
           angle_change = ifelse(last_event != 'none',
                                 abs(angle - lag(angle)),
                                 0),
           on_goal = ifelse(event == 'shot' & outcome == 'successful', 1, 0))
  
  # Subsequent events info
  d2 <- d %>%
    filter(event == 'shot') %>%
    mutate(rebound_shot = ifelse((lead(time) - time) <= rebound_time_cutoff &
                                   (lead(time) - time) >= 0 &
                                   lead(date) == date &
                                   lead(game) == game &
                                   lead(period) == period &
                                   lead(team) == team,
                                 1, 0),
           creates_rebound_shot = ifelse(creates_rebound == 1 &
                                           rebound_shot == 1,
                                         1, 0))
  d2 <- d2 %>% select(id, rebound_shot, creates_rebound_shot)
  d <- left_join(d, d2, by = 'id')
  
  d2 <- d %>%
    filter(event != 'penaltydrawn') %>%
    filter((event == 'save') | (lag(event) == 'save')) %>%
    mutate(after_save = ifelse(event == 'save' & period == lead(period) & game == lead(game), lead(event), NA),
           after_save_home_team = ifelse(!is.na(after_save), lead(is_home_team), NA),
           after_save_adj_x = ifelse(!is.na(after_save), lead(adj_x), NA))
  d2 <- d2 %>% select(id, after_save, after_save_home_team, after_save_adj_x)
  d <- left_join(d, d2, by = 'id')
  
  d2 <- d %>%
    filter(event != 'penaltydrawn') %>%
    filter((event == 'shot') | (lag(event) == 'shot')) %>%
    mutate(after_shot = ifelse(event == 'shot' & period == lead(period) & game == lead(game), lead(event), NA),
           after_shot_home_team = ifelse(!is.na(after_shot), lead(is_home_team), NA),
           after_shot_adj_x = ifelse(!is.na(after_shot), lead(adj_x), NA))
  d2 <- d2 %>% select(id, after_shot, after_shot_home_team, after_shot_adj_x)
  d <- left_join(d, d2, by = 'id')
  
  d2 <- d %>%
    filter((event == 'shot') | (event == 'save')) %>%
    mutate(play_stopped = ifelse(event == 'shot' &
                                   lead(event) == 'save' &
                                   ((lead(after_save) == 'faceoff') |
                                      (lead(after_save) == 'penalty' & lead(after_save_home_team) == lead(is_home_team))),
                                 1, 0),
           play_stopped = ifelse(event == 'shot' &
                                   ((after_shot == 'faceoff') |
                                      (after_shot == 'penalty' & after_shot_home_team == is_home_team)),
                                 1, play_stopped),
           in_zone = ifelse(play_stopped == 0 &
                              ((lead(after_save_home_team) == lead(is_home_team) & lead(after_save_adj_x) <= -24) |
                                 (lead(after_save_home_team) != lead(is_home_team) & lead(after_save_adj_x) >= 24)),
                            1, 0),
           in_zone = ifelse(play_stopped == 0 &
                              ((after_shot_home_team == is_home_team & after_shot_adj_x <= -24) |
                                 (after_shot_home_team != is_home_team & after_shot_adj_x >= 24)),
                            1, in_zone),
           out_zone = ifelse(play_stopped == 0 &
                               ((lead(after_save_home_team) == lead(is_home_team) & lead(after_save_adj_x) > -24) |
                                  (lead(after_save_home_team) != lead(is_home_team) & lead(after_save_adj_x) < 24)),
                             1, 0),
           out_zone = ifelse(play_stopped == 0 &
                               ((after_shot_home_team == is_home_team & after_shot_adj_x > -24) |
                                  (after_shot_home_team != is_home_team & after_shot_adj_x < 24)),
                             1, out_zone))
  d2 <- d2 %>% select(id, play_stopped, in_zone, out_zone)
  d <- left_join(d, d2, by = 'id')
  
  # Clean
  drop <- c('after_save', 'after_save_home_team', 'after_save_adj_x',
            'after_shot', 'after_shot_home_team', 'after_shot_adj_x')
  
  d <- d %>%
    mutate(creates_rebound_shot = ifelse((event == 'shot') & !is.na(creates_rebound_shot), creates_rebound_shot, 0),
           on_goal = ifelse((event == 'shot') & !is.na(on_goal), on_goal, 0),
           play_stopped = ifelse((event == 'shot') & !is.na(play_stopped), play_stopped, 0),
           rebound_shot = ifelse((event == 'shot') & !is.na(rebound_shot), rebound_shot, 0),
           in_zone = ifelse((event == 'shot') & !is.na(in_zone), in_zone, 0),
           out_zone = ifelse((event == 'shot') & !is.na(out_zone), out_zone, 0)) %>%
    select(-one_of(drop))
  
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
    select(id, season_type, date, game,
           team, opposing_team, is_home_team, period,
           strength_state, time, event,
           dist, abs_angle,
           goal_diff, time_since_last, last_event, angle_change,
           shooter, shooter_position, goalie, shot_type, shot_aim,
           on_goal, goal, rebound_shot, play_stopped, in_zone, out_zone)
  
  return(d)
}

# Extract pass data from processed play-by-play
extract.pass.data <- function(d) {
  passes_receptions_shots <- d %>%
    filter(event %in% c('shot', 'pass', 'reception'))
  
  shots_after_receptions <- d %>%
    filter(lag(event) == 'reception' &
             lag(period) == period) %>%
    filter(event == 'shot')
  
  shots_successful_passes_receptions <- passes_receptions_shots %>%
    filter((event == 'shot' & id %in% shots_after_receptions$id) |
             (event %in% c('reception', 'pass') & outcome == 'successful'))
  
  # Invalid pass ids
  invalid_pass_ids <- shots_successful_passes_receptions %>%
    filter((event == 'pass') & ((lead(event) != 'reception') |
                                  (lead(event, 2) != 'shot') |
                                  (id > (max(id) - 2)))) %>%
    select(id)
  
  # Invalid reception ids
  invalid_reception_ids <- shots_successful_passes_receptions %>%
    filter((event == 'reception') & ((lead(event) != 'shot') |
                                       (lag(event) != 'pass') |
                                       (id == max(id)))) %>%
    select(id)
  
  # Valid passes, receptions, shots
  valid_passes_receptions_shots <- shots_successful_passes_receptions %>%
    filter(!(id %in% invalid_pass_ids$id) & !(id %in% invalid_reception_ids$id))
  
  valid_passes <- valid_passes_receptions_shots %>%
    filter(event %in% c('pass', 'shot')) %>%
    mutate(resulting_shot_value = ifelse(event == 'pass' & lead(event) == 'shot',
                                         lead(xgoal_all_no_last),
                                         NA)) %>%
    filter(event == 'pass')
  
  # Passes
  passes <- valid_passes %>%
    arrange(desc(resulting_shot_value)) %>%
    mutate(danger = ifelse(resulting_shot_value > quantile(resulting_shot_value, 0.8),
                           'high',
                           'low'))
  
  # Players
  players <- passes %>%
    rename(player = shooter,
           player_position = shooter_position) %>%
    filter(danger == 'high') %>%
    group_by(player) %>%
    summarise(count = n(),
              position = unique(player_position))
  
  return(list(passes = passes, players = players))
}

# Extract offensive zone entry data from play-by-play and summary
extract.ozone.entry.data <- function(d, summ) {
  ozone_entries <- d %>%
    filter(event == 'controlledentry' & outcome == 'successful')
  
  ozone_entries <- ozone_entries[grepl('carry', ozone_entries$type),]
  
  ozone_entries <- ozone_entries %>% filter(y > -40 & y < 0)
  
  players <- ozone_entries %>%
    rename(player = shooter,
           player_position = shooter_position) %>%
    group_by(player) %>%
    summarise(count = n(),
              position = unique(player_position))
  
  summ <- summ %>%
    rename(player = Player) %>%
    group_by(player) %>%
    summarise(toi = sum(TOI))
  
  summ <- summ %>% select(player, toi)
  
  players <- left_join(players, summ, by = 'player')
  
  players$count_per_60 = players$count / (players$toi / (60 * 60))
  
  players <- players %>% ungroup()
  ozone_entries <- ozone_entries %>% ungroup()
  
  return(list(ozone_entries = ozone_entries, players = players))
}

# Helper function for use when creating rink regions
format.region <- function(d, name) {
  df <- d %>%
    as.data.frame() %>%
    setNames(c('x', 'y')) %>%
    mutate(region = name)
  
  return(df)
}

# Assign regions to data
get.regions <- function(d) {
  inner_slot <- cbind(c( -7,   7,   7,  -7,  -7),
                      c(-69, -69, -89, -89, -69))
  right_slot <- cbind(c(-22,  -7,  -7, -22, -22),
                      c(-54, -54, -89, -69, -54))
  left_slot <- cbind(c(  7,  22,  22,   7,   7),
                     c(-54, -54, -69, -89, -54))
  high_slot <- cbind(c( -7,   7,   7,  -7,  -7),
                     c(-54, -54, -69, -69, -54))
  
  right_outside <- cbind(c(-42.5, -22, -22,  -7, -36.75, -42.5, -42.5),
                         c(  -54, -54, -69, -89,    -89,   -72,   -54))
  left_outside <- cbind(c( 22, 42.5, 42.5, 36.75,   7,  22,  22),
                        c(-54,  -54,  -72,   -89, -89, -69, -54))
  
  right_point <- cbind(c(-42.5,  -7,  -7, -42.5, -42.5),
                       c(  -26, -26, -54,   -54,   -26))
  left_point <- cbind(c(  7, 42.5, 42.5,   7,   7),
                      c(-26,  -26,  -54, -54, -26))
  center_point <- cbind(c( -7,   7,   7,  -7,  -7),
                        c(-26, -26, -54, -54, -26))
  
  regions <- rbind(format.region(inner_slot, 'inner.slot'),
                   format.region(right_slot, 'right.slot'),
                   format.region(left_slot, 'left.slot'),
                   format.region(high_slot, 'high.slot'),
                   format.region(right_outside, 'right.outside'),
                   format.region(left_outside, 'left.outside'),
                   format.region(right_point, 'right.point'),
                   format.region(left_point, 'left.point'),
                   format.region(center_point, 'center.point'))
  
  s_inner_slot <- Polygons(list(Polygon(inner_slot)), 'inner.slot')
  s_right_slot <- Polygons(list(Polygon(right_slot)), 'right.slot')
  s_left_slot <- Polygons(list(Polygon(left_slot)), 'left.slot')
  s_high_slot <- Polygons(list(Polygon(high_slot)), 'high.slot')
  
  s_right_outside <- Polygons(list(Polygon(right_outside)), 'right.outside')
  s_left_outside <- Polygons(list(Polygon(left_outside)), 'left.outside')
  
  s_right_point <- Polygons(list(Polygon(right_point)), 'right.point')
  s_left_point <- Polygons(list(Polygon(left_point)), 'left.point')
  s_center_point <- Polygons(list(Polygon(center_point)), 'center.point')
  
  s_regions <- SpatialPolygons(list(s_inner_slot,
                                    s_right_slot,
                                    s_left_slot,
                                    s_high_slot,
                                    s_right_outside,
                                    s_left_outside,
                                    s_right_point,
                                    s_left_point,
                                    s_center_point))
  
  s_regions_df <- SpatialPolygonsDataFrame(s_regions,
                                           data.frame(region = c('inner.slot',
                                                                 'right.slot',
                                                                 'left.slot',
                                                                 'high.slot',
                                                                 'right.outside',
                                                                 'left.outside',
                                                                 'right.point',
                                                                 'left.point',
                                                                 'center.point'),
                                                      row.names = c('inner.slot',
                                                                    'right.slot',
                                                                    'left.slot',
                                                                    'high.slot',
                                                                    'right.outside',
                                                                    'left.outside',
                                                                    'right.point',
                                                                    'left.point',
                                                                    'center.point')))
  points <- d[, c('x','y')]
  coordinates(points) <- ~x+y
  result <- over(points, s_regions_df)
  
  d <- d %>%
    mutate(region = result$region,
           region = ifelse(is.na(region), 'other', region))
  
  return(list(d = d, regions = regions))
}

# Find region-specific information for rebounds
get.regions.rebounds <- function(d, regions, unblocked = FALSE) {
  if (!unblocked) {
    d2 <- d %>%
      filter(event == 'shot') %>%
      mutate(rebound_shot_xg = ifelse(creates_rebound_shot == 1,
                                      lead(xgoal_all_no_last), NA))
    d2 <- d2 %>% select(id, rebound_shot_xg)
    d <- left_join(d, d2, by = 'id')
    
    d <- d %>%
      mutate(rebound_shot_xg = ifelse(event == 'shot', rebound_shot_xg, NA))
    
  } else {
    d2 <- d %>%
      filter((event == 'shot') & !(type %in% c('slotblocked', 'outsideblocked'))) %>%
      mutate(rebound_shot_xg = ifelse(creates_rebound_shot == 1,
                                      lead(xgoal_unblocked_no_last), NA))
    d2 <- d2 %>% select(id, rebound_shot_xg)
    d <- left_join(d, d2, by = 'id')
    
    d <- d %>%
      mutate(rebound_shot_xg = ifelse(event == 'shot', rebound_shot_xg, NA))
  }
  
  d$region_rebound_prob <- NA
  d$region_rebound_shot_prob <- NA
  d$region_rebound_shot_xg <- NA
  d$region_shot_count <- NA
  d$region_xgoal_all <- NA
  d$region_xgoal_all_no_last <- NA
  
  d$region_rebound_prob_unblocked <- NA
  d$region_rebound_shot_prob_unblocked <- NA
  d$region_rebound_shot_xg_unblocked <- NA
  d$region_shot_count_unblocked <- NA
  d$region_xgoal_unblocked <- NA
  d$region_xgoal_unblocked_no_last <- NA
  
  for (region_name in region_names) {
    if (!unblocked) {
      # All shot attempts
      region_rebound_prob <- sum(d$creates_rebound[(d$event == 'shot') & (d$region == region_name)]) / nrow(d[(d$event == 'shot') & (d$region == region_name), ])
      region_rebound_shot_prob <- sum(d$creates_rebound_shot[(d$event == 'shot') & (d$region == region_name)]) / nrow(d[(d$event == 'shot') & (d$region == region_name), ])
      region_rebound_shot_xg <- mean(d$rebound_shot_xg[(d$event == 'shot') & (d$region == region_name)], na.rm = TRUE)
      region_xgoal_all <- mean(d$xgoal_all[(d$event == 'shot') & (d$region == region_name)], na.rm = TRUE)
      region_xgoal_all_no_last <- mean(d$xgoal_all_no_last[(d$event == 'shot') & (d$region == region_name)], na.rm = TRUE)
      
      regions$region_rebound_prob[regions$region == region_name] <- region_rebound_prob
      regions$region_rebound_shot_prob[regions$region == region_name] <- region_rebound_shot_prob
      regions$region_rebound_shot_xg[regions$region == region_name] <- region_rebound_shot_xg
      regions$region_xgoal_all[regions$region == region_name] <- region_xgoal_all
      regions$region_xgoal_all_no_last[regions$region == region_name] <- region_xgoal_all_no_last
      
      d$region_rebound_prob[d$region == region_name] <- region_rebound_prob
      d$region_rebound_shot_prob[d$region == region_name] <- region_rebound_shot_prob
      d$region_rebound_shot_xg[d$region == region_name] <- region_rebound_shot_xg
      d$region_xgoal_all[d$region == region_name] <- region_xgoal_all
      d$region_xgoal_all_no_last[d$region == region_name] <- region_xgoal_all_no_last
      
      region_shot_count <- nrow(d[(d$event == 'shot') & (d$region == region_name), ])
      d$region_shot_count[d$region == region_name] <- region_shot_count
      regions$region_shot_count[regions$region == region_name] <- region_shot_count
      
    } else {
      # Unblocked shot attempts
      region_rebound_prob_unblocked <- sum(d$creates_rebound[(d$event == 'shot') & !(d$type %in% c('slotblocked', 'outsideblocked')) & (d$region == region_name)]) / nrow(d[(d$event == 'shot') & !(d$type %in% c('slotblocked', 'outsideblocked')) & (d$region == region_name), ])
      region_rebound_shot_prob_unblocked <- sum(d$creates_rebound_shot[(d$event == 'shot') & !(d$type %in% c('slotblocked', 'outsideblocked')) & (d$region == region_name)]) / nrow(d[(d$event == 'shot') & !(d$type %in% c('slotblocked', 'outsideblocked')) & (d$region == region_name), ])
      region_rebound_shot_xg_unblocked <- mean(d$rebound_shot_xg[(d$event == 'shot') & !(d$type %in% c('slotblocked', 'outsideblocked')) & (d$region == region_name)], na.rm = TRUE)
      region_xgoal_unblocked <- mean(d$xgoal_unblocked[(d$event == 'shot') & (d$region == region_name)], na.rm = TRUE)
      region_xgoal_unblocked_no_last <- mean(d$xgoal_unblocked_no_last[(d$event == 'shot') & (d$region == region_name)], na.rm = TRUE)
      
      regions$region_rebound_prob_unblocked[regions$region == region_name] <- region_rebound_prob_unblocked
      regions$region_rebound_shot_prob_unblocked[regions$region == region_name] <- region_rebound_shot_prob_unblocked
      regions$region_rebound_shot_xg_unblocked[regions$region == region_name] <- region_rebound_shot_xg_unblocked
      regions$region_xgoal_unblocked[regions$region == region_name] <- region_xgoal_unblocked
      regions$region_xgoal_unblocked_no_last[regions$region == region_name] <- region_xgoal_unblocked_no_last
      
      d$region_rebound_prob_unblocked[d$region == region_name] <- region_rebound_prob_unblocked
      d$region_rebound_shot_prob_unblocked[d$region == region_name] <- region_rebound_shot_prob_unblocked
      d$region_rebound_shot_xg_unblocked[d$region == region_name] <- region_rebound_shot_xg_unblocked
      d$region_xgoal_unblocked[d$region == region_name] <- region_xgoal_unblocked
      d$region_xgoal_unblocked_no_last[d$region == region_name] <- region_xgoal_unblocked_no_last
      
      region_shot_count_unblocked <- nrow(d[(d$event == 'shot') & !(d$type %in% c('slotblocked', 'outsideblocked')) & (d$region == region_name), ])
      d$region_shot_count_unblocked[d$region == region_name] <- region_shot_count_unblocked
      regions$region_shot_count_unblocked[regions$region == region_name] <- region_shot_count_unblocked
    }
  }
  
  if (!unblocked) {
    regions$region_rebound_total <- regions$region_rebound_prob * regions$region_rebound_shot_prob * regions$region_rebound_shot_xg
    d$region_rebound_total <- d$region_rebound_prob * d$region_rebound_shot_prob * d$region_rebound_shot_xg
  } else {
    regions$region_rebound_total_unblocked <- regions$region_rebound_prob_unblocked * regions$region_rebound_shot_prob_unblocked * regions$region_rebound_shot_xg_unblocked
    d$region_rebound_total_unblocked <- d$region_rebound_prob_unblocked * d$region_rebound_shot_prob_unblocked * d$region_rebound_shot_xg_unblocked
  }
  
  return(list(d = d, regions = regions))
}

# Get shots
get.shots <- function(d, unblocked = FALSE) {
  d <- d %>% filter(event == 'shot')
  
  if (unblocked) {
    d <- d %>% filter(!(type %in% c('slotblocked', 'outsideblocked')))
  }
  
  return(d)
}

# Get shots that generate rebounds
get.shots.generating.rebounds <- function(d, unblocked = FALSE) {
  return(get.shots(d, unblocked = unblocked) %>% filter(creates_rebound == 1))
}

# Get shots that generate rebound shots
get.shots.generating.rebound.shots <- function(d, unblocked = FALSE) {
  return(get.shots.generating.rebounds(d, unblocked = unblocked) %>% filter(creates_rebound_shot == 1))
}