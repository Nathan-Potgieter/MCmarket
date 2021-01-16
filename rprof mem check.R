pacman::p_load(copula, tidyverse, lubridate, profmem)

p <- profmem({
corr <- P2p(diag(50))
N <- 10
k <- 300
})

p <- profmem({
copula <- tCopula(param = corr, dispstr = "un", dim = 50, df = 3)
})

p <- profmem({
start_date <- Sys.Date()
dates <- rmsfuns::dateconverter(StartDate = start_date,
                                EndDate = start_date %m+% lubridate::days(k-1),
                                Transform = "alldays")
})


p <- profmem({
marginal_dist_model <- list(mu = 0, sd = 1)
args <- tibble(Asset = glue::glue("V{1:N}")) %>%
    mutate(mean = marginal_dist_model$mu,
           sd = marginal_dist_model$sd,
           ncp = marginal_dist_model$ncp,
           df = marginal_dist_model$df,
           lambda = marginal_dist_model$lambda,
           p = marginal_dist_model$p,
           q = marginal_dist_model$q)
})


# Probable culprit: but dont know how to get around....
p <- profmem({
rcop <- rCopula(copula, n = k)
}) #1.3Mb total

p <- profmem({
rcop_tidy <- rcop %>% as_tibble() %>%
    mutate(date = dates) %>%
    gather(Asset, Value, -date)
}) # 0.5Mb total


p <- profmem({
rcop_tidy_join <- rcop_tidy %>%
    left_join(., args, by = "Asset")
}) #1.4Mb total

p <- profmem({
rcop_tidy_group <- rcop_tidy_join %>%
    group_by(Asset)
}) #0.32Mb total

p <- profmem({
rcop_tidy_mutate <- rcop_tidy_group %>%
        mutate(Return = qnorm(Value, mean = mean, sd = sd))
}) #0.61Mb total

p <- profmem({
rcop_tidy_select <- rcop_tidy_mutate %>%
    select(date, Asset, Return)
}) #0.007Mb total

# --------------------------------
# Ideally I want something like this
# --------------------------------
p <- profmem({
market <-
map_dfr(1:N, ~rCopula(copula, n = k) %>%  as_tibble() %>%
        mutate(date = dates) %>%
        gather(Asset, Value, -date) %>%
            left_join(., args, by = "Asset") %>%
            group_by(Asset) %>%
            mutate(Return = qnorm(Value, mean = mean, sd = sd)) %>%
            select(date, Asset, Return), .id = "Universe")
})
object.size(x = mcmarket) %>% print(units = "Mb") #0.5Mb
gc()


# ==================================
# Possible alternative for rCopula?
# Works with N = 10000
# ===================================

library(mvtnorm)
library(propagate)
cov <- diag(50) %>% cor2cov(var = c(1:50))
N <- 100
k <- 300
start_date <- Sys.Date()
dates <- rmsfuns::dateconverter(StartDate = start_date,
                                EndDate = start_date %m+% lubridate::days(k-1),
                                Transform = "alldays")
mcmarket <-
    map_dfr(1:N, ~rmvnorm(k, sigma = cov, method = "chol", mean = 1:50) %>% as_tibble() %>%
    mutate(date = dates) %>%
    gather(Asset, Return, -date), .id = "Universe")
gc()

mcmarket %>% filter(Universe == 1) %>%
    ggplot() +
    geom_line(aes(x=date,y=Return,color=Asset))

rmvt(300, sigma = cov, df = 5, method = "chol") %>% as_tibble() %>%
    mutate(date = dates) %>%
    gather(Asset, Return, -date)


