source('functions/modeling.R')
source('functions/prepare.R')
source('functions/rink.R')
source('functions/visualize.R')

# Read data
files.available = TRUE

file.names = c('data/pbp.rds',
               'data/pbp_all.rds',
               'data/regions_all.rds',
               'data/pbp_unblocked.rds',
               'data/regions_unblocked.rds',
               'data/models.rds',
               'data/metrics.rds')

for (file.name in file.names) {
  if (!file.exists(file.name)) {
    files.available = FALSE
    break
  }
}

if (files.available == TRUE) {
  pbp <- readRDS('data/pbp.rds')
  
  pbp_all <- readRDS('data/pbp_all.rds')
  regions_all <- readRDS('data/regions_all.rds')
  
  pbp_unblocked <- readRDS('data/pbp_unblocked.rds')
  regions_unblocked <- readRDS('data/regions_unblocked.rds')
  
  models <- readRDS('data/models.rds')
  metrics <- readRDS('data/metrics.rds')
  
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
  
  unblocked.shots <- get.shots(pbp, unblocked = TRUE)
  unblocked.shots <- process.model.data(unblocked.shots)
  
  model.goal.unblocked <- model(unblocked.shots, unblocked.shots$goal)
  unblocked.shots$xgoal.unblocked <- predict(model.goal.unblocked, unblocked.shots, type = 'response')
  a.goal.unblocked <- auc(as.integer(as.logical(unblocked.shots$goal)), unblocked.shots$xgoal.unblocked)
  ll.goal.unblocked <- logLoss(unblocked.shots$goal, unblocked.shots$xgoal.unblocked)
  
  unblocked.shots$xgoal.unblocked.no.last <- predict(model.goal.unblocked,
                                                     unblocked.shots %>% mutate(last.event = 'none'),
                                                     allow.new.levels = TRUE,
                                                     type = 'response')
  a.goal.unblocked.no.last <- auc(as.integer(as.logical(unblocked.shots$goal)), unblocked.shots$xgoal.unblocked.no.last)
  ll.goal.unblocked.no.last <- logLoss(unblocked.shots$goal, unblocked.shots$xgoal.unblocked.no.last)
  
  models <- list(model.goal.all, model.goal.unblocked)
  metrics <- data.frame(model = c('goal.all', 'goal.all.no.last', 'goal.unblocked', 'goal.unblocked.no.last'),
                        train.auc = c(a.goal.all, a.goal.all.no.last, a.goal.unblocked, a.goal.unblocked.no.last),
                        train.log.loss = c(ll.goal.all, ll.goal.all.no.last, ll.goal.unblocked, ll.goal.unblocked.no.last))
  
  # Rebounds
  pbp <- left_join(pbp, all.shots %>% select(id, xgoal.all, xgoal.all.no.last), by = 'id')
  pbp <- left_join(pbp, unblocked.shots %>% select(id, xgoal.unblocked, xgoal.unblocked.no.last), by = 'id')
  
  retval <- get.regions(pbp)
  retval <- get.regions.rebounds(retval$d, retval$regions, unblocked = FALSE)
  pbp_all <- retval$d
  regions_all <- retval$regions
  
  retval <- get.regions(pbp)
  retval <- get.regions.rebounds(retval$d, retval$regions, unblocked = TRUE)
  pbp_unblocked <- retval$d
  regions_unblocked <- retval$regions
  
  rm(retval)
  
  # Save
  saveRDS(pbp, 'data/pbp.rds')
  
  saveRDS(pbp_all, 'data/pbp_all.rds')
  saveRDS(regions_all, 'data/regions_all.rds')
  
  saveRDS(pbp_unblocked, 'data/pbp_unblocked.rds')
  saveRDS(regions_unblocked, 'data/regions_unblocked.rds')
  
  saveRDS(models, 'data/models.rds')
  saveRDS(metrics, 'data/metrics.rds')
}

# Regions
regions.plot.all <- plot.regions(regions_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'regions_all', '.jpg'), 
       plot = regions.plot.all,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

regions.plot.unblocked <- plot.regions(regions_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'regions_unblocked', '.jpg'),
       plot = regions.plot.unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# xG
xg.plot.all <- plot.xg(regions_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'xg_all', '.jpg'), 
       plot = xg.plot.all,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg.plot.unblocked <- plot.xg(regions_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'xg_unblocked', '.jpg'),
       plot = xg.plot.unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# Rebounds
overall.rebounds.all <- plot.rebounds1(pbp_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'rebounds_all', '.jpg'), 
       plot = overall.rebounds.all,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

overall.rebounds.unblocked <- plot.rebounds1(pbp_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'rebounds_unblocked', '.jpg'),
       plot = overall.rebounds.unblocked,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg.rebounds.all <- plot.rebounds2(pbp_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'xg_rebounds_all', '.jpg'), 
       plot = xg.rebounds.all,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg.rebounds.unblocked <- plot.rebounds2(pbp_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'xg_rebounds_unblocked', '.jpg'),
       plot = xg.rebounds.unblocked,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)