#-------------------
# MCmarket workflow
#-------------------
pacman::p_load(MCmarket, dplyr, purrr, ggcorrplot, ggplot2)

# Generate ad hoc correlation matrix
cor <- gen_corr(D = 20,
                Clusters = "overlapping",
                Num_Layers = 3,
                Num_Clusters = c(10, 5, 4))
cor %>% ggcorrplot::ggcorrplot()

#-----------------
# Using sim_market
#-----------------
set.seed(12345)
market <- sim_market(cor,
                      k = 500,
                      mv_dist = "norm",
                      left_cop_weight = 0.25,
                      left_cop_param = 5,
                      ts_model = list(mu = 0.0005, alpha = 0.99,
                                      gamma = 0.5, delta = 1.98))
market %>%
    arrange(date) %>%
    group_by(Asset) %>%
    mutate(cum_ret = cumprod(1 + Return)*100) %>%
    ungroup() %>%
    ggplot(aes(date, cum_ret, color = Asset)) +
    geom_line() +
    facet_wrap(~Asset, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none")

#-----------------
# Using mc_market
#-----------------
set.seed(12345)
MCmarkets <-
    mc_market(corr = diag(20),
              N = 5,
              k = 500,
              mv_dist = "norm",
              left_cop_weight = 0.25,
              marginal_dist = "norm",
              ts_model = list(mu = 0.0005,
                              ar = 0.0005,
                              alpha = 0.99,
                              gamma = 0.0002,
                              delta = 1.98))
MCmarkets %>%
    group_by(Asset, Universe) %>%
    arrange(date) %>%
    mutate(cum_ret = cumprod(1 + Return)*100) %>%
    ggplot() +
    geom_line(aes(x = date, y = cum_ret, color = Universe), size = 1, alpha = 0.5) +
    facet_wrap(~Asset, scales = "free_y") +
    labs(title = "Ensemble of Cumulative Returns",
         subtitle = "100 Realizations for a Market of 20 Assets") +
    theme_bw()+
    theme(legend.position = "none")

# Simulating Monte Carlo markets with a progress bar

# Setting random mean and sd
set.seed(1251)
sd <- rnorm(20, mean =  0.1, sd = 0.03)
mean <- rnorm(20, mean =  0.01, sd = 0.01)

N <- 100
pb <- dplyr::progress_estimated(N)

MCmarkets2 <- map_dfr(1:N,
                      ~sim_market_with_progress(corr = cor,
                                                k = 300,
                                                mv_dist = "t",
                                                mv_df = 3,
                                                marginal_dist = "norm",
                                                marginal_dist_model = list(mu = mean, sd = sd)),
                      .id = "Universe")

object.size(x = MCmarkets2) %>% print(units = "Mb")

# Note that by increasing the left_cop_weight from 0 - 0.5
# the sd declines by approx 40%. Continuing to increase it
# from 0.5 - 1, then returns sd back to set value. This
# relationship seems highly non-linear. May be a serious problem.
set.seed(12345)
inno <-
    sim_market(cor,
               k = 10000,
               mv_dist = "t",
               mv_df = 4,  # Degrees of freedom for multivariate t distribution
               left_cop_weight = 0, # Weight attributed to Clayton copula
               left_cop_param = 4,
               marginal_dist = "sgt",
               marginal_dist_model = list(mu = 0, # Mean
                                          sd = 1,  # Standard Deviation
                                          lambda = -0.2, # Skewness
                                          p = 2,  # Kurtosis - smaller => larger kurtosis
                                          q = 1000)) # Kurtosis - smaller => larger kurtosis

inno %>%group_by(Asset) %>% mutate(sd = sd(Return)) %>%
    filter(date == first(date))



