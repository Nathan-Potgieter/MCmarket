#-------------------
# MCmarket workflow
#-------------------
pacman::p_load(MCmarket, dplyr, purrr, ggcorrplot, ggplot2)

# First look at the diagonal correlation matrix

corr_1 <- diag(50)
corr_1 %>% ggcorrplot(hc.order = TRUE, title = "Diagonal Matrix")

# Correlation matrix with no clusters

corr_2 <- gen_corr(D = 50, clusters = "none")
corr_2 %>% ggcorrplot(title = "No Clusters")
0.9^(5-1)

# Correlation matrix with 5 clusters

corr_3 <- gen_corr(D = 50, clusters = "non-overlapping", num_clusters = 5)
eigen_3 <- eigen(corr_3)
corr_3 %>% ggcorrplot(hc.order = TRUE, title = "Five Clusters")


# Correlation matrix with 10, 5 and 2 overlapping clusters

corr_4 <- gen_corr(D = 50, clusters = "overlapping", num_clusters = c(10,5,2), num_layers = 3)
eigen_4 <- eigen(corr_4)
corr_4 %>% ggcorrplot(hc.order = TRUE, title = "Overlapping Clusters")


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



