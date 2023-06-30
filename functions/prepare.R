library(readr)
library(dplyr)
library(sp)

# Time cutoffs and defaults in seconds
rebound.time.cutoff <- 3
last.event.time.cutoff <- 5
time.since.last.default <- 99

# Regions used
region.names <- c('inner.slot', 'right.slot', 'left.slot', 'high.slot',
                  'right.outside', 'left.outside',
                  'right.point', 'left.point', 'center.point',
                  'other')

# Rename data columns
process.names <- function(d) {
  d <- d %>%
    rename(season.type = seasonstage,
           team = teamname,
           opposing.team = opposingteamname,
           is.home.team = ishomegame,
           possession.team = teaminpossession,
           current.possession = currentpossession,
           strength.state = strengthstate,
           team.skaters.on.ice = teamskatersonicecount,
           opposing.team.skaters.on.ice = opposingteamskatersonicecount,
           time = compiledgametime,
           goal.diff = scoredifferential,
           adj.x = xadjcoord,
           adj.y = yadjcoord,
           shooter = player,
           shooter.position = playerprimaryposition,
           own.goalie = goalie,
           goalie = opposing_goalie,
           event = eventname,
           xg.all.attempts = xg_all_attempts,
           creates.rebound = createsrebound,
           shot.type = shottype,
           shot.aim = shotaim)
  
  # d <- d %>%
  #   select()
  
  return(d)
}

# Convert coordinates into plot-able format
process.coords <- function(d, half.rink = TRUE, horizontal = FALSE) {
  x <- d$adj.x
  y <- d$adj.y
  
  if (half.rink == TRUE) {
    x <- -abs(x)
  }
  
  if (horizontal == TRUE) {
    d$x <- x
    d$y <- ifelse(d$adj.x > 0, -y, y)
  } else {
    d$x <- ifelse(d$adj.x > 0, y, -y)
    d$y <- x
  }
  
  return(d)
}

# Data cleaning, etc.
process.data <- function(d) {
  d <- process.names(d)
  d <- process.coords(d, half.rink = TRUE, horizontal = FALSE)
  
  # Shot info
  d <- d %>%
    mutate(id = row_number(),
           own.goalie = ifelse(!is.na(own.goalie), own.goalie, ''),
           goalie = ifelse(!is.na(goalie), goalie, ''),
           dist = sqrt(x^2 + (y + 89)^2),
           angle = atan(x / (y + 89)) * (180 / pi),
           abs.angle = abs(angle),
           time.since.last = ifelse((time - lag(time)) >= 0 &
                                      date == lag(date) &
                                      game == lag(game) &
                                      period == lag(period),
                                    time - lag(time),
                                    time.since.last.default),
           time.since.last = ifelse(!is.na(time.since.last),
                                    time.since.last,
                                    time.since.last.default),
           last.event = ifelse(time.since.last <= last.event.time.cutoff,
                               lag(event),
                               'none'),
           last.event = ifelse(is.na(last.event), 'none', last.event),
           last.event.team = ifelse(last.event == 'none', NA, lag(team)),
           angle.change = ifelse(last.event != 'none',
                                 abs(angle - lag(angle)),
                                 0),
           on.goal = ifelse(event == 'shot' & outcome == 'successful', 1, 0))
  
  # Subsequent events info
  d2 <- d %>%
    filter(event == 'shot') %>%
    mutate(rebound.shot = ifelse((lead(time) - time) <= rebound.time.cutoff &
                                   (lead(time) - time) >= 0 &
                                   lead(date) == date &
                                   lead(game) == game &
                                   lead(period) == period &
                                   lead(team) == team,
                                 1, 0),
           creates.rebound.shot = ifelse(creates.rebound == 1 &
                                           rebound.shot == 1,
                                         1, 0))
  d2 <- d2 %>% select(id, rebound.shot, creates.rebound.shot)
  d <- left_join(d, d2, by = 'id')
  
  d2 <- d %>%
    filter(event != 'penaltydrawn') %>%
    filter((event == 'save') | (lag(event) == 'save')) %>%
    mutate(after.save = ifelse(event == 'save' & period == lead(period) & game == lead(game), lead(event), NA),
           after.save.home.team = ifelse(!is.na(after.save), lead(is.home.team), NA),
           after.save.adj.x = ifelse(!is.na(after.save), lead(adj.x), NA))
  d2 <- d2 %>% select(id, after.save, after.save.home.team, after.save.adj.x)
  d <- left_join(d, d2, by = 'id')
  
  d2 <- d %>%
    filter(event != 'penaltydrawn') %>%
    filter((event == 'shot') | (lag(event) == 'shot')) %>%
    mutate(after.shot = ifelse(event == 'shot' & period == lead(period) & game == lead(game), lead(event), NA),
           after.shot.home.team = ifelse(!is.na(after.shot), lead(is.home.team), NA),
           after.shot.adj.x = ifelse(!is.na(after.shot), lead(adj.x), NA))
  d2 <- d2 %>% select(id, after.shot, after.shot.home.team, after.shot.adj.x)
  d <- left_join(d, d2, by = 'id')
  
  d2 <- d %>%
    filter((event == 'shot') | (event == 'save')) %>%
    mutate(play.stopped = ifelse(event == 'shot' &
                                   lead(event) == 'save' &
                                   ((lead(after.save) == 'faceoff') |
                                      (lead(after.save) == 'penalty' & lead(after.save.home.team) == lead(is.home.team))),
                                 1, 0),
           play.stopped = ifelse(event == 'shot' &
                                   ((after.shot == 'faceoff') |
                                      (after.shot == 'penalty' & after.shot.home.team == is.home.team)),
                                 1, play.stopped),
           in.zone = ifelse(play.stopped == 0 &
                              ((lead(after.save.home.team) == lead(is.home.team) & lead(after.save.adj.x) <= -24) |
                                 (lead(after.save.home.team) != lead(is.home.team) & lead(after.save.adj.x) >= 24)),
                            1, 0),
           in.zone = ifelse(play.stopped == 0 &
                              ((after.shot.home.team == is.home.team & after.shot.adj.x <= -24) |
                                 (after.shot.home.team != is.home.team & after.shot.adj.x >= 24)),
                            1, in.zone),
           out.zone = ifelse(play.stopped == 0 &
                               ((lead(after.save.home.team) == lead(is.home.team) & lead(after.save.adj.x) > -24) |
                                  (lead(after.save.home.team) != lead(is.home.team) & lead(after.save.adj.x) < 24)),
                             1, 0),
           out.zone = ifelse(play.stopped == 0 &
                               ((after.shot.home.team == is.home.team & after.shot.adj.x > -24) |
                                  (after.shot.home.team != is.home.team & after.shot.adj.x < 24)),
                             1, out.zone))
  d2 <- d2 %>% select(id, play.stopped, in.zone, out.zone)
  d <- left_join(d, d2, by = 'id')
  
  # Clean
  drop <- c('after.save', 'after.save.home.team', 'after.save.adj.x',
            'after.shot', 'after.shot.home.team', 'after.shot.adj.x')
  
  d <- d %>%
    mutate(creates.rebound.shot = ifelse((event == 'shot') & !is.na(creates.rebound.shot), creates.rebound.shot, 0),
           on.goal = ifelse((event == 'shot') & !is.na(on.goal), on.goal, 0),
           play.stopped = ifelse((event == 'shot') & !is.na(play.stopped), play.stopped, 0),
           rebound.shot = ifelse((event == 'shot') & !is.na(rebound.shot), rebound.shot, 0),
           in.zone = ifelse((event == 'shot') & !is.na(in.zone), in.zone, 0),
           out.zone = ifelse((event == 'shot') & !is.na(out.zone), out.zone, 0)) %>%
    select(-one_of(drop))
  
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
    select(id, season.type, date, game,
           team, opposing.team, is.home.team, period,
           strength.state, time, event,
           dist, abs.angle,
           goal.diff, time.since.last, last.event, angle.change,
           shooter, shooter.position, goalie, shot.type, shot.aim,
           on.goal, goal, rebound.shot, play.stopped, in.zone, out.zone)
  
  return(d)
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
  inner.slot <- cbind(c( -7,   7,   7,  -7,  -7),
                      c(-69, -69, -89, -89, -69))
  right.slot <- cbind(c(-22,  -7,  -7, -22, -22),
                      c(-54, -54, -89, -69, -54))
  left.slot <- cbind(c(  7,  22,  22,   7,   7),
                     c(-54, -54, -69, -89, -54))
  high.slot <- cbind(c( -7,   7,   7,  -7,  -7),
                     c(-54, -54, -69, -69, -54))
  
  right.outside <- cbind(c(-42.5, -22, -22,  -7, -36.75, -42.5, -42.5),
                         c(  -54, -54, -69, -89,    -89,   -72,   -54))
  left.outside <- cbind(c( 22, 42.5, 42.5, 36.75,   7,  22,  22),
                        c(-54,  -54,  -72,   -89, -89, -69, -54))
  
  right.point <- cbind(c(-42.5,  -7,  -7, -42.5, -42.5),
                       c(  -26, -26, -54,   -54,   -26))
  left.point <- cbind(c(  7, 42.5, 42.5,   7,   7),
                      c(-26,  -26,  -54, -54, -26))
  center.point <- cbind(c( -7,   7,   7,  -7,  -7),
                        c(-26, -26, -54, -54, -26))
  
  regions <- rbind(format.region(inner.slot, 'inner.slot'),
                   format.region(right.slot, 'right.slot'),
                   format.region(left.slot, 'left.slot'),
                   format.region(high.slot, 'high.slot'),
                   format.region(right.outside, 'right.outside'),
                   format.region(left.outside, 'left.outside'),
                   format.region(right.point, 'right.point'),
                   format.region(left.point, 'left.point'),
                   format.region(center.point, 'center.point'))
  
  s.inner.slot <- Polygons(list(Polygon(inner.slot)), 'inner.slot')
  s.right.slot <- Polygons(list(Polygon(right.slot)), 'right.slot')
  s.left.slot <- Polygons(list(Polygon(left.slot)), 'left.slot')
  s.high.slot <- Polygons(list(Polygon(high.slot)), 'high.slot')
  
  s.right.outside <- Polygons(list(Polygon(right.outside)), 'right.outside')
  s.left.outside <- Polygons(list(Polygon(left.outside)), 'left.outside')
  
  s.right.point <- Polygons(list(Polygon(right.point)), 'right.point')
  s.left.point <- Polygons(list(Polygon(left.point)), 'left.point')
  s.center.point <- Polygons(list(Polygon(center.point)), 'center.point')
  
  s.regions <- SpatialPolygons(list(s.inner.slot,
                                    s.right.slot,
                                    s.left.slot,
                                    s.high.slot,
                                    s.right.outside,
                                    s.left.outside,
                                    s.right.point,
                                    s.left.point,
                                    s.center.point))
  
  s.regions.df <- SpatialPolygonsDataFrame(s.regions,
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
  result <- over(points, s.regions.df)
  
  d <- d %>%
    mutate(region = result$region,
           region = ifelse(is.na(region), 'other', region))
  
  return(list(d = d, regions = regions))
}

# Find region-specific information for rebounds
get.regions.rebounds <- function(d, regions) {
  d2 <- d %>%
    filter(event == 'shot') %>%
    mutate(rebound.shot.xg = ifelse(creates.rebound.shot == 1,
                                    lead(xgoal.all.no.last), NA))
  d2 <- d2 %>% select(id, rebound.shot.xg)
  d <- left_join(d, d2, by = 'id')
  
  d <- d %>%
    mutate(rebound.shot.xg = ifelse(event == 'shot', rebound.shot.xg, NA))
  
  d$region.rebound.prob <- NA
  d$region.rebound.shot.prob <- NA
  d$region.rebound.shot.xg <- NA
  d$region.shot.count <- NA
  d$region.xgoal.all <- NA
  d$region.xgoal.all.no.last <- NA
  
  for (region.name in region.names) {
    # All shot attempts
    region.rebound.prob <- sum(d$creates.rebound[(d$event == 'shot') & (d$region == region.name)]) / nrow(d[(d$event == 'shot') & (d$region == region.name), ])
    region.rebound.shot.prob <- sum(d$creates.rebound.shot[(d$event == 'shot') & (d$region == region.name)]) / nrow(d[(d$event == 'shot') & (d$region == region.name), ])
    region.rebound.shot.xg <- mean(d$rebound.shot.xg[(d$event == 'shot') & (d$region == region.name)], na.rm = TRUE)
    region.xgoal.all <- mean(d$xgoal.all[(d$event == 'shot') & (d$region == region.name)], na.rm = TRUE)
    region.xgoal.all.no.last <- mean(d$xgoal.all.no.last[(d$event == 'shot') & (d$region == region.name)], na.rm = TRUE)
    
    regions$region.rebound.prob[regions$region == region.name] <- region.rebound.prob
    regions$region.rebound.shot.prob[regions$region == region.name] <- region.rebound.shot.prob
    regions$region.rebound.shot.xg[regions$region == region.name] <- region.rebound.shot.xg
    regions$region.xgoal.all[regions$region == region.name] <- region.xgoal.all
    regions$region.xgoal.all.no.last[regions$region == region.name] <- region.xgoal.all.no.last
    
    d$region.rebound.prob[d$region == region.name] <- region.rebound.prob
    d$region.rebound.shot.prob[d$region == region.name] <- region.rebound.shot.prob
    d$region.rebound.shot.xg[d$region == region.name] <- region.rebound.shot.xg
    d$region.xgoal.all[d$region == region.name] <- region.xgoal.all
    d$region.xgoal.all.no.last[d$region == region.name] <- region.xgoal.all.no.last
    
    region.shot.count <- nrow(d[(d$event == 'shot') & (d$region == region.name), ])
    d$region.shot.count[d$region == region.name] <- region.shot.count
    regions$region.shot.count[regions$region == region.name] <- region.shot.count
  }
  
  regions$region.rebound.total <- regions$region.rebound.prob * regions$region.rebound.shot.prob * regions$region.rebound.shot.xg
  d$region.rebound.total <- d$region.rebound.prob * d$region.rebound.shot.prob * d$region.rebound.shot.xg
  
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
  return(get.shots(d, unblocked = unblocked) %>% filter(creates.rebound == 1))
}

# Get shots that generate rebound shots
get.shots.generating.rebound.shots <- function(d, unblocked = FALSE) {
  return(get.shots.generating.rebounds(d, unblocked = unblocked) %>% filter(creates.rebound.shot == 1))
}