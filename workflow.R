#-------------------
# MCmarket workflow
#-------------------
pacman::p_load(MCmarket, dplyr, purrr, ggplot2)

# Generate ad hoc correlation matrix
cor <- gen_corr(D = 20,
                Clusters = "overlapping",
                Num_Layers = 3,
                Num_Clusters = c(10, 5, 4))
cor %>% ggcorrplot::ggcorrplot()


# Simulating innovations
set.seed(1234)
inno <- sim_inno(cor,
                 k = 252,
                 mv_dist = "t",
                 mv_df = 5,
                 left_cop_weight = 0.25,
                 left_cop_param = 4,
                 marginal_dist = "sgt",
                 marginal_dist_model = list(mu = 0,
                                            sigma = 1,
                                            lambda = -0.1,
                                            p = 2,
                                            q = Inf)
                 )

# ------------------
# Applying sim_garch
#-------------------
market <- map_dfc(inno, ~sim_garch(.x,
                                   model = list(mu = 0.005, alpha = 0.99,
                                                gamma = 0.001, delta = 1.98)))

# Or if applied to a single series
r_timeseries <-
    sim_garch(rnorm(500),
              model = list(ar = c(0.55,0.4), alpha = 0.99, gamma = 0.001),
              simple = FALSE)
r_timeseries$y %>% plot(type = "l")


#-----------------
# Using sim_market
#-----------------
set.seed(12345)
market2 <- sim_market(cor,
                      k = 500,
                      mv_dist = "norm",
                      left_cop_weight = 0.25,
                      left_cop_param = 5,
                      ts_model = list(mu = 0.0005, alpha = 0.99,
                                      gamma = 0.5, delta = 1.98))
market2 %>%
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


set.seed(12345)
inno <-
    sim_inno(cor,
             k = 1000,
             mv_dist = "t",
             mv_df = 4,  # Degrees of freedom for multivariate t distribution
             left_cop_weight = 0, # Weight attributed to Clayton copula
             left_cop_param = 4,
             marginal_dist = "sgt",
             marginal_dist_model = list(mu = 0.02, # Mean
                                        sd = 0.03,  # Standard Deviation
                                        lambda = -0.2, # Skewness
                                        p = 2,  # Kurtosis - smaller => larger kurtosis
                                        q = 1000)) # Kurtosis - smaller => larger kurtosis

inno %>% map(~sd(.x))
