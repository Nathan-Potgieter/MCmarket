#============================================
# This script walks through chunks of code from the
# sim_market function and measures maximum memory usage along the way
#============================================

pacman::p_load(MCmarket, tidyverse, lubridate)

# ellipCopula memory usage
gc1 <- gc(reset = TRUE)
Ecop <- ellipCopula(family = "t",  # 0.5Mb
                    dispstr = "un",
                    df = mv_df,
                    param = P2p(corr),
                    dim = N)
gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))

# rCopula memory usage #2.2Mb
gc1 <- gc(reset = TRUE)
data <- rCopula(k, Ecop)
gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))


# Creating a date vector (0.4Mb) and make tidy (5.1Mb); total 5.5Mb: memory usage
gc1 <- gc(reset = TRUE)

start_date <- Sys.Date()
dates <- rmsfuns::dateconverter(StartDate = start_date,
                                EndDate = start_date %m+% lubridate::days(k-1),
                                Transform = "alldays")
data_tidy <- as_tibble(data) %>%
    purrr::set_names(glue::glue("Asset_{1:ncol(data)}")) %>%
    mutate(date = dates) %>%
    gather(Asset, Value, -date)

gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))




# Converting Uniform marginal distributions to norm, t or sgt. 10.7Mb
gc1 <- gc(reset = TRUE)

if (marginal_dist == "norm" & is.null(marginal_dist_model)) marginal_dist_model <- list(mu=0, sd = 1)
if (marginal_dist == "t" & is.null(marginal_dist_model))  marginal_dist_model <- list(ncp = 0, df = 5)
args <- tibble(Asset = glue::glue("Asset_{1:N}")) %>%
    mutate(mean = marginal_dist_model$mu,
           sd = marginal_dist_model$sd,
           ncp = marginal_dist_model$ncp,
           df = marginal_dist_model$df,
           lambda = marginal_dist_model$lambda,
           p = marginal_dist_model$p,
           q = marginal_dist_model$q)
data_norm <- data_tidy %>% left_join(., args, by = "Asset") %>%
    group_by(Asset) %>%  arrange(date) %>%
    mutate(Return =  qnorm(Value, mean, sd)) %>%
    select(date, Asset, Return)

gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))

# ===============================================
# This is not very important for now!!!!!!!!!!!
# as I am not using this option for MY THESIS
# ==============================================
# Inducing mean and/or variance persistence: 83 Mb

gc1 <- gc(reset = TRUE)
ts_model <- list() # required input
# Tibble with with garh parameters and defaults
ts_args <- tibble(Asset = glue::glue("Asset_{1:N}")) %>%
    mutate(omega = if (is.null(ts_model$omega)) {5e-04} else {ts_model$omega},
           alpha = if (is.null(ts_model$alpha)) {0} else {ts_model$alpha},
           gamma = if (is.null(ts_model$gamma)) {0}  else {ts_model$gamma},
           beta = if (is.null(ts_model$beta)) {0} else {ts_model$beta},
           mu = if (is.null(ts_model$mu)) {0} else {ts_model$mu},   #changed form NULL to 0
           ar = if (is.null(ts_model$ar)) {0} else {ts_model$ar},
           ma = if (is.null(ts_model$ma)) {0} else {ts_model$ma},
           delta = if (is.null(ts_model$delta)) {2} else {ts_model$delta})


# Inducing garch properties
data <- data_norm %>% left_join(., ts_args, by = "Asset") %>%
    arrange(date) %>% group_by(Asset) %>%
    mutate(Return = sim_garch(innovations = Return,
                              omega = omega,
                              alpha = alpha,
                             gamma = gamma,
                              beta = beta,
                              mu = mu,
                              ar = ar,
                              ma = ma,
                              delta = delta)) %>% na.omit() %>%
    select(date, Asset, Return)

gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))


# sim_market memory usage: 22.2Mb (if ts_model = NULL) or 102.6Mb (if ts_model = list() )
gc1 <- gc(reset = TRUE)

market <- sim_market(diag(50), k = 500, ts_model = NULL) # the code

gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))
print(object.size(market), units = "Mb") # 0.7Mb

# ======================================
# Checking memory usage when mapping
# ======================================

# Mapping usage
# sim_market memory usage 109.3Mb
gc1 <- gc(reset = TRUE)
market <- purrr::map_dfr(1:10, ~sim_market(diag(50), k = 500)) # the code
gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))
print(object.size(market), units = "Mb") # 6.7mb


# rcopula memory usage
gc1 <- gc(reset = TRUE)
data <- purrr::map_dfr(1:10, ~rCopula(k, Ecop))  #22Mb
gc2 <- gc()
cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))
print(object.size(data), units = "Mb") # 0.2Mb


