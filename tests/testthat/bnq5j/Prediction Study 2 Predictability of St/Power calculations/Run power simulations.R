library(tidyverse)
#-------------------------------------------------------------------------------
# Data generating function

gen_predictability_data <- function(
    mu, tausq, delta, sigmasq,
    N = 100, p_total = 5, p_c = NA, p_d = NA,
    J_C = 18L, J_D = 16L
) {
  
  # comparison prompt predictability
  ac <- mu * (mu * (1 - mu) / tausq - 1)
  bc <- (1 - mu) * (mu * (1 - mu) / tausq - 1)
  pi_C <- rbeta(J_C, shape1 = ac, shape2 = bc)
  
  # Decline Effects prompt predictability  
  ad <- (mu + delta) * ((mu + delta) * (1 - mu - delta) / tausq - 1)
  bd <- (1 - mu - delta) * ((mu + delta) * (1 - mu - delta) / tausq - 1)
  pi_star_D <- rbeta(J_D, shape1 = ad, shape2 = bd)
  pi_D <- pi_star_D - mean(pi_star_D) + delta + mean(pi_C)
  
  # first C, then D
  pi_vec <- pmin(0.99, pmax(0.01, c(pi_C, pi_D)))
  gamma <- qnorm(pi_vec) * sqrt(1 + sigmasq)
  
  # person skills
  epsilon <- rnorm(N, sd = sqrt(sigmasq))
  
  # sampling scheme
  if (!is.na(p_total)) {
    items <- replicate(N, sample(1:(J_C + J_D), size = p_total, replace = FALSE))
  } else {
    items_c <- replicate(N, sample(1:J_C, size = p_c, replace = FALSE))
    items_d <- replicate(N, sample(J_C + 1:J_D, size = p_d, replace = FALSE))
    items <- rbind(items_c, items_d)
    p_total <- p_c + p_d
  }
  
  dat <- data.frame(
    i = rep(1:N, each = p_total),
    j = as.vector(items)
  )
  
  dat$x <- dat$j > J_C
  dat$epsilon <- epsilon[dat$i]
  dat$gamma <- gamma[dat$j]
  dat$p_ij <- pnorm(dat$epsilon + dat$gamma)
  dat$Y <- rbinom(n = nrow(dat), size = 1L, prob = dat$p_ij)
  
  dat
}

# dat <- gen_predictability_data(
#   mu = 0.3, tausq = 0.2^2, delta = 0.1, sigmasq = 0.1^2, 
#   N = 10000, p_total = 20, J_C = 100, J_D = 100
# )
# 
# item_summary <- 
#   dat %>% 
#   group_by(x, j) %>%
#   summarise(
#     gamma = unique(gamma),
#     pi = pnorm(unique(gamma) / sqrt(1 + 0.1^2)),
#     Ep_ij = mean(p_ij),
#     pr = mean(Y),
#     .groups = "drop_last"
#   )
# 
# item_summary %>%
#   summarise(
#     across(c(gamma, pi, Ep_ij, pr), mean)
#   )
# 
# ggplot(item_summary, aes(x = pi, y = Ep_ij, color = x)) + 
#   geom_point() +
#   geom_abline(slope = 1) + 
#   scale_x_continuous(limits = c(0,1)) + 
#   scale_y_continuous(limits = c(0,1)) + 
#   theme_minimal() + 
#   theme(legend.position = "none")
# 
# ggplot(item_summary, aes(x = pi, y = pr, color = x)) + 
#   geom_point() +
#   geom_abline(slope = 1) + 
#   scale_x_continuous(limits = c(0,1)) + 
#   scale_y_continuous(limits = c(0,1)) + 
#   theme_minimal() + 
#   theme(legend.position = "none")

#-------------------------------------------------------------------------------
# Estimation functions


estimate_marginal <- function(dat) {
  require(clubSandwich, quietly = TRUE)
  lm_fit <- lm(Y ~ 0 + factor(j), data = dat)
  linear_contrast(
    lm_fit, vcov = "CR2", cluster = dat$i,
    contrasts = list(marginal = matrix(rep(c(-1/18, 1/16), c(18,16)), nrow = 1)),
    level = 0.9 # 1-sided test
  )
}

estimate_within <- function(dat) {
  xi <- with(dat, split(x, i))
  yi <- with(dat, split(Y, i))
  both <- sapply(xi, function(x) length(unique(x)) == 2)
  di <- mapply(function(x, y) mean(y[x]) - mean(y[!x]), x = xi[both], y = yi[both])
  lm_fit <- lm(di ~ 1)
  res <- cbind(
    Coef = "within",
    as.data.frame(summary(lm_fit)$coefficients[,c("Estimate","Std. Error"),drop=FALSE]),
    df = lm_fit$df.residual,
    as.data.frame(confint(lm_fit, level = 0.9))
  )
  colnames(res) <- c("Coef","Est","SE","df","CI_L","CI_U")
  res
}

#-------------------------------------------------------------------------------
# Simulation driver

sim_predictability <- function(
    reps, 
    mu, tausq, delta, sigmasq, delta0 = 0.05,
    N = 100, p_total = 5, p_c = NULL, p_d = NULL,
    seed = NULL
) {
  
  if (!is.null(seed)) set.seed(seed)
  
  res <- map_dfr(1:reps, ~ {
    dat <- gen_predictability_data(
      mu = mu, tausq = tausq, delta = delta, sigmasq = sigmasq,
      N = N, p_total = p_total, p_c = p_c, p_d = p_d
    )
    
    bind_rows(
      estimate_marginal(dat),
      estimate_within(dat)
    )
  })
  
  res %>%
    group_by(Coef) %>%
    summarise(
      mean = mean(Est),
      sd = sd(Est),
      reject = mean(CI_U < delta0 )
    )
  
}

# sim_predictability(
#   reps = 500,
#   mu = 0.3, tausq = 0.25^2,
#   delta = 0.0, sigmasq = 0.04^2, delta0 = 0.05,
#   N = 100, p_c = 3, p_d = 3
# )

#-------------------------------------------------------------------------------
# Simulation design

design_fac <- list(
  N = seq(200, 2400, 200),
  mu = seq(0.2, 0.8, 0.1),
  tausq = c(0.10, 0.15, 0.20, 0.25)^2,
  delta = 0, 
  sigmasq = c(0.02, 0.04, 0.1, 0.2)^2,
  delta0 = 0.05,
  p_c = 3L,
  p_d = 3L
)

design_conditions <- 
  cross_df(design_fac) %>%
  mutate(
    reps = 500L
  )

design_conditions %>%
  mutate(f = mu * (1 - mu) / tausq - 1) %>%
  filter(f < 1)

nrow(design_conditions)
head(design_conditions)

#-------------------------------------------------------------------------------
# Execute simulations
library(tictoc)
library(future)
library(furrr)
set.seed(20221102)

plan(multisession)

tic()

sim_results <- 
  design_conditions %>%
  mutate(
    res = future_pmap(., sim_predictability, .options = furrr_options(seed = TRUE))
  ) %>%
  unnest(res)

toc()

sim_results

saveRDS(sim_results, file = "predictability survey/Power calculations/simulated power rates.rds")
