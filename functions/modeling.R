library(lme4)
library(splines)
library(ModelMetrics)

# Generalized linear mixed model used for xG and xG*
model <- function(x, y) {
  y <- as.logical(y)
  
  fit <- glmer(y ~ 1 + ns(dist, df = 6) + ns(abs_angle, df = 6) +
                 (1 | goal_diff) + ns(time_since_last, df = 6) +
                 (1 | last_event) +
                 (1 | shooter) + (1 | goalie) + (1 | shot_type),
               data = x, family = binomial(),
               control = glmerControl(optimizer = 'Nelder_Mead',
                                      optCtrl = list(maxfun = 2e5)))
  
  return(fit)
}