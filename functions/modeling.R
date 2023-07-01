library(lme4)
library(splines)
library(ModelMetrics)

# Generalized linear mixed model used for xG and xG*
model <- function(x, y) {
  y <- as.logical(y)
  
  fit <- glmer(y ~ 1 + ns(dist, df = 6) + ns(abs.angle, df = 6) +
                 (1 | goal.diff) + ns(time.since.last, df = 6) +
                 (1 | last.event) +
                 (1 | shooter) + (1 | goalie) + (1 | shot.type),
               data = x, family = binomial(),
               control = glmerControl(optimizer = 'Nelder_Mead',
                                      optCtrl = list(maxfun = 2e5)))
  
  return(fit)
}