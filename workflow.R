#-------------------
# MCmarket workflow
#-------------------
pacman::p_load(MCmarket, tidyverse, ggcorrplot)


#---------------------------------------------
# The first step is to think about what kind of
# market it is that you want to simulate. So lets
# start with the correlation matrix
#---------------------------------------------


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

# An empirical rally correlation matrix
corr_5 <- corr_mats$cor_rally[[1]]
eigen_5 <- eigen(corr_5)
corr_5 %>% ggcorrplot(hc.order = TRUE, title = "Overlapping Clusters")

#===========
# sim_market
#===========
#----------------------------------------------------------
# Market 1:
# Using sim_market to simulate a single market with
# the empirical rally correlation matrix and the same
# ar + garch model with drift and normally distributed innovations
# for each asset.
#---------------------------------------------------------
set.seed(12345)
market <- sim_market(
    corr_5, # empirical rally correlation matrix
    k = 500,
    mv_dist = "norm", # normal innovations
    ts_model = list(mu = 0.0015, # Drift term
                    alpha = 0.5,
                    beta = 0.1,
                    ar = 0.1)
)
# plotting output
market %>%
    arrange(date) %>%
    group_by(Asset) %>%
    mutate(cum_ret = cumprod(1 + Return)*100) %>%
    ungroup() %>%
    ggplot(aes(date, cum_ret, color = Asset)) +
    geom_line() +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "Market 1",
         y = "log(Cumulative Return)") +
    scale_y_log10()
#-----------------------------------------------------------------
# Market 2:
# Using sim_market to simulate a single market with
# the empirical rally correlation matrix, a multivariate t distribution
# with 10 degrees of freedom, a different ar + garch model and t distributed
# innovations with 4 degrees of freedom.
#-----------------------------------------------------------------
set.seed(12345)
market2 <- sim_market(
    corr_5,# empirical rally correlation matrix
    k = 500,
    mv_dist = "t",# t innovations
    mv_df = 10, # multivariate degrees of freedom
    marginal_dist = "t",
    marginal_dist_model = list(ncp = 0, df = 4), # innovation 4 degrees of freedom
        ts_model = list( # Note how ts parameters parameters are set randomly
            mu = runif(50, 0.001, 0.0025),
            alpha = runif(50, 0, 0.1),
            beta = runif(50, 0, 0.1),
            ar = runif(50, 0, 0.01)
        )
)
# plotting output
market2 %>%
    arrange(date) %>%
    group_by(Asset) %>%
    mutate(cum_ret = cumprod(1 + Return)*100) %>%
    ungroup() %>%
    ggplot(aes(date, cum_ret, color = Asset)) +
    geom_line() +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "Market 2",
         y = "log(Cumulative Return)") +
    scale_y_log10()

#==================
# Using mc_market
#==================
# Simulating Market 1 20 times.
set.seed(12345)
MCmarket1 <- mc_market(
    N = 20, # Number of simulations
    corr_5, # empirical rally correlation matrix
    k = 500,
    mv_dist = "norm", # normal innovations
    ts_model = list(mu = 0.0015, # Drift term
                    alpha = 0.5,
                    beta = 0.1,
                    ar = 0.1),
    list = FALSE   # Using list = FALSE for easy plotting
)
MCmarket1 %>%
    group_by(Asset, Universe) %>%
    arrange(date) %>%
    mutate(cum_ret = cumprod(1 + Return)*100) %>%
    ggplot() +
    geom_line(aes(x = date, y = cum_ret, color = Asset), size = 0.5, alpha = 0.5) +
    facet_wrap(~Universe, scales = "free_y") +
    labs(title = "Ensemble of Cumulative Returns",
         subtitle = "20 Realizations for a Market 1") +
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



