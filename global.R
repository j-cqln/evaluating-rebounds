library(readr)
library(readxl)

source('functions/modeling.R')
source('functions/prepare.R')
source('functions/rink.R')
source('functions/visualize.R')

# Read data
files_available = TRUE

file_names = c('data/pbp.rds',
               'data/passes.rds',
               'data/passes_players.rds',
               'data/ozone_entries.rds',
               'data/ozone_entries_players.rds',
               'data/pbp_all.rds',
               'data/regions_all.rds',
               'data/pbp_unblocked.rds',
               'data/regions_unblocked.rds',
               'data/models.rds',
               'data/metrics.rds')

if (all(file.exists(file_names))) {
  pbp <- readRDS('data/pbp.rds')
  
  passes <- readRDS('data/passes.rds')
  passes_players <- readRDS('data/passes_players.rds')
  
  ozone_entries <- readRDS('data/ozone_entries.rds')
  ozone_entries_players <- readRDS('data/ozone_entries_players.rds')
  
  pbp_all <- readRDS('data/pbp_all.rds')
  regions_all <- readRDS('data/regions_all.rds')
  
  pbp_unblocked <- readRDS('data/pbp_unblocked.rds')
  regions_unblocked <- readRDS('data/regions_unblocked.rds')
  
  models <- readRDS('data/models.rds')
  metrics <- readRDS('data/metrics.rds')
  
} else {
  season_summary <- read_excel("data/DATA_FILE_NAME.xlsx")
  
  pbp <- read_csv('data/DATA_FILE_NAME.csv')
  pbp <- process.data(pbp)
  
  # xG
  all_shots <- get.shots(pbp, unblocked = FALSE)
  all_shots <- process.model.data(all_shots)
  
  model_goal_all <- model(all_shots, all_shots$goal)
  all_shots$xgoal_all <- predict(model_goal_all, all_shots, type = 'response')
  a_goal_all <- auc(as.integer(as.logical(all_shots$goal)), all_shots$xgoal_all)
  ll_goal_all <- logLoss(all_shots$goal, all_shots$xgoal_all)
  
  all_shots$xgoal_all_no_last <- predict(model_goal_all,
                                         all_shots %>% mutate(last_event = 'none'),
                                         allow.new.levels = TRUE,
                                         type = 'response')
  a_goal_all_no_last <- auc(as.integer(as.logical(all_shots$goal)), all_shots$xgoal_all_no_last)
  ll_goal_all_no_last <- logLoss(all_shots$goal, all_shots$xgoal_all_no_last)
  
  unblocked_shots <- get.shots(pbp, unblocked = TRUE)
  unblocked_shots <- process.model.data(unblocked_shots)
  
  model_goal_unblocked <- model(unblocked_shots, unblocked_shots$goal)
  unblocked_shots$xgoal_unblocked <- predict(model_goal_unblocked, unblocked_shots, type = 'response')
  a_goal_unblocked <- auc(as.integer(as.logical(unblocked_shots$goal)), unblocked_shots$xgoal_unblocked)
  ll_goal_unblocked <- logLoss(unblocked_shots$goal, unblocked_shots$xgoal_unblocked)
  
  unblocked_shots$xgoal_unblocked_no_last <- predict(model_goal_unblocked,
                                                     unblocked_shots %>% mutate(last_event = 'none'),
                                                     allow.new.levels = TRUE,
                                                     type = 'response')
  a_goal_unblocked_no_last <- auc(as.integer(as.logical(unblocked_shots$goal)), unblocked_shots$xgoal_unblocked_no_last)
  ll_goal_unblocked_no_last <- logLoss(unblocked_shots$goal, unblocked_shots$xgoal_unblocked_no_last)
  
  models <- list(model_goal_all, model_goal_unblocked)
  metrics <- data.frame(model = c('goal_all', 'goal_all_no_last', 'goal_unblocked', 'goal_unblocked_no_last'),
                        train.auc = c(a_goal_all, a_goal_all_no_last, a_goal_unblocked, a_goal_unblocked_no_last),
                        train.log.loss = c(ll_goal_all, ll_goal_all_no_last, ll_goal_unblocked, ll_goal_unblocked_no_last))
  
  # Rebounds
  pbp <- left_join(pbp, all_shots %>% select(id, xgoal_all, xgoal_all_no_last), by = 'id')
  pbp <- left_join(pbp, unblocked_shots %>% select(id, xgoal_unblocked, xgoal_unblocked_no_last), by = 'id')
  
  retval <- get.regions(pbp)
  retval <- get.regions.rebounds(retval$d, retval$regions, unblocked = FALSE)
  pbp_all <- retval$d
  regions_all <- retval$regions
  
  retval <- get.regions(pbp)
  retval <- get.regions.rebounds(retval$d, retval$regions, unblocked = TRUE)
  pbp_unblocked <- retval$d
  regions_unblocked <- retval$regions
  
  # Passes
  retval <- extract.pass.data(pbp)
  passes <- retval$passes
  passes_players <- retval$players
  
  # Offensive zone entries
  retval <- extract.ozone.entry.data(pbp, season_summary)
  ozone_entries <- retval$ozone_entries
  ozone_entries_players <- retval$players
  
  rm(retval)
  
  # Save
  saveRDS(pbp, 'data/pbp.rds')
  
  saveRDS(passes, 'data/passes.rds')
  saveRDS(passes_players, 'data/passes_players.rds')
  
  saveRDS(ozone_entries, 'data/ozone_entries.rds')
  saveRDS(ozone_entries_players, 'data/ozone_entries_players.rds')
  
  saveRDS(pbp_all, 'data/pbp_all.rds')
  saveRDS(regions_all, 'data/regions_all.rds')
  
  saveRDS(pbp_unblocked, 'data/pbp_unblocked.rds')
  saveRDS(regions_unblocked, 'data/regions_unblocked.rds')
  
  saveRDS(models, 'data/models.rds')
  saveRDS(metrics, 'data/metrics.rds')
}

# Regions
regions_plot_all <- plot.regions(regions_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'regions_all', '.jpg'), 
       plot = regions_plot_all,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

regions_plot_unblocked <- plot.regions(regions_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'regions_unblocked', '.jpg'),
       plot = regions_plot_unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# xG
xg_plot_all <- plot.xg(regions_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'xg_all', '.jpg'), 
       plot = xg_plot_all,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg_plot_unblocked <- plot.xg(regions_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'xg_unblocked', '.jpg'),
       plot = xg_plot_unblocked,
       width = 5,
       height = 6.5,
       units = 'in',
       dpi = 72)

# Rebounds
overall_rebounds_all <- plot.rebounds1(pbp_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'rebounds_all', '.jpg'), 
       plot = overall_rebounds_all,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

overall_rebounds_unblocked <- plot.rebounds1(pbp_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'rebounds_unblocked', '.jpg'),
       plot = overall_rebounds_unblocked,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg_rebounds_all <- plot.rebounds2(pbp_all, unblocked = FALSE)

ggsave(filename = paste0('www/', 'xg_rebounds_all', '.jpg'), 
       plot = xg_rebounds_all,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

xg_rebounds_unblocked <- plot.rebounds2(pbp_unblocked, unblocked = TRUE)

ggsave(filename = paste0('www/', 'xg_rebounds_unblocked', '.jpg'),
       plot = xg_rebounds_unblocked,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

# Passes
passes_plot <- plot.passes(passes, passes_players)

ggsave(filename = paste0('www/', 'passes', '.jpg'),
       plot = passes_plot,
       width = 20,
       height = 6.5,
       units = 'in',
       dpi = 72)

# Offensive zone entries
ozone_entries_plot <- plot.ozone.entries(ozone_entries, ozone_entries_players)

ggsave(filename = paste0('www/', 'ozone_entries_map', '.jpg'),
       plot = ozone_entries_plot[[1]],
       width = 20,
       height = 4.5,
       units = 'in',
       dpi = 72)

ggsave(filename = paste0('www/', 'ozone_entries_players', '.jpg'),
       plot = ozone_entries_plot[[2]],
       width = 7,
       height = 6.5,
       units = 'in',
       dpi = 72)