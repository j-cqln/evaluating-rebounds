source('functions/modeling.R')
source('functions/prepare.R')
source('functions/rink.R')
source('functions/visualize.R')

# Read data
if (file.exists('data/pbp.rds') &&
    file.exists('data/regions.rds') &&
    file.exists('data/models.rds') &&
    file.exists('data/metrics.rds')) {
  
  pbp <- readRDS('data/pbp.rds')
  regions <- readRDS('data/regions.rds')
  
  models <- readRDS('data/models.rds')
  metrics <- saveRDS('data/metrics.rds')
  
} else {
  pbp <- read_csv('data/DATA_FILE_NAME.csv')
  pbp <- process.data(pbp)
  
  # xG for use with rebounds
  all.shots <- get.shots(pbp, unblocked = FALSE)
  all.shots <- process.model.data(all.shots)
  
  model.goal.all <- model(all.shots, all.shots$goal)
  all.shots$xgoal.all <- predict(model.goal.all, all.shots, type = 'response')
  a.goal.all <- auc(as.integer(as.logical(all.shots$goal)), all.shots$xgoal.all)
  ll.goal.all <- logLoss(all.shots$goal, all.shots$xgoal.all)
  
  all.shots$xgoal.all.no.last <- predict(model.goal.all,
                                         all.shots %>% mutate(last.event = 'none'),
                                         allow.new.levels = TRUE,
                                         type = 'response')
  a.goal.all.no.last <- auc(as.integer(as.logical(all.shots$goal)), all.shots$xgoal.all.no.last)
  ll.goal.all.no.last <- logLoss(all.shots$goal, all.shots$xgoal.all.no.last)

  models <- list(model.goal.all)
  metrics <- data.frame(model = c('goal.all', 'goal.all.no.last'),
                        train.auc = c(a.goal.all, a.goal.all.no.last),
                        train.log.loss = c(ll.goal.all, ll.goal.all.no.last))
  
  # Rebounds
  pbp <- left_join(pbp, all.shots %>% select(id, xgoal.all, xgoal.all.no.last), by = 'id')

  retval <- get.regions(pbp)
  retval <- get.regions.rebounds(retval$d, retval$regions)
  pbp <- retval$d
  regions <- retval$regions
  
  rm(retval)
  
  # Save
  saveRDS(pbp, 'data/pbp.rds')
  saveRDS(regions, 'data/regions.rds')
  
  saveRDS(models, 'data/model.rds')
  saveRDS(metrics, 'data/metrics.rds')
}

# Regions
regions.plot.all <- plot.regions(regions)

ggsave(filename = paste0('www/', 'regions_all', '.jpg'), 
       plot = regions.plot.all,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# xG
xg.plot.all <- plot.xg(regions)

ggsave(filename = paste0('www/', 'xg_all', '.jpg'), 
       plot = xg.plot.all,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# Rebounds
overall.rebounds.all <- plot.rebounds1(pbp)

ggsave(filename = paste0('www/', 'rebounds_all', '.jpg'), 
       plot = overall.rebounds.all,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg.rebounds.all <- plot.rebounds2(pbp)

ggsave(filename = paste0('www/', 'xg_rebounds_all', '.jpg'), 
       plot = xg.rebounds.all,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)