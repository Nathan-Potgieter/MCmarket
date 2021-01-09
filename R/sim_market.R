#' @title sim_market
#' @description This function produces a series of returns for an asset market with a
#' given correlation matrix. The user can choose between the multivariate t and normal
#' distributions and adjust the markets left tail dependency by weighting in the Clayton copula.
#' The univariate asset return distributions can also be set to normal, student-t or sgt
#' distributed. Finally, mean and variance persistence can be induced via the parameters of an
#' ARMA + APARCH model.
#' @note  It is suggested that, if the ts_model is used, then the marginal distributions be set
#' to list(mu = 0, sd = 1). These attributes are better off being set in the ts_model argument.
#' @param corr a correlation matrix that the simulated date will adhere to. Note that the
#' number of variables simulated is equal to the number of columns in the correlation matrix.
#' @param k a positive integer indicating the number of time periods to simulate. Note that
#' the number of periods generated is actually equal to k + 5 as these extra observations are
#' needed when applying time series properties to the data.
#' @param mv_dist a string specifying the multivariate distribution. Can be one of c("norm", "t")
#' referring to the multivariate normal and t distributions respectively. Default is 3.
#' @param mv_df degrees of freedom of the multivariate distribution, required when mv_dist = "t".
#' @param left_cop_weight a positive value between zero and one indicating the weight applied to
#' the Clayton copula when creating the multivariate distribution. Note that a value between zero
#' and one essentially generates a hybrid distribution between the chosen mv_dist and the Clayton
#' copula. Therefore, the greater the left_cop_weight the less the data will reflect the correlation
#' structure. Default is set to 0.
#' @param left_cop_param a positive value indicating the parameter of the Clayton copula. Default is 4.
#' @param marginal_dist a string variable specifying the univariate distribution of each variable. Can
#' be one of c("norm", "t", "sgt") referring to the normal, student-t and skewed-generalized-t
#' distributions respectively. Default is "norm".
#' @param  marginal_dist_model list containing the relevant parameters for the chosen marginal_dist.
#' marginal_dist = "norm" accepts a mean and standard deviation with the respective defaults
#' list(mu = 0, sigma = 1). marginal_dist = "t" accepts the non-centrality and degrees of freedom arguments,
#' default values are list(mu = 0, df = 5). marginal_dist = "sgt" accepts the mean, sd, lambda, p
#' and q parameters list(mu = 0, sigma = 1, lambda, p, q). Note lambda, p and q have no defaults
#' and must therefore be set by the user.
#' @param ts_model a list containing various ARIMA + APGARCH model parameters allowing one to specify
#' the time series properties of the simulated returns. Note that parameter combinations resulting
#' in non-stationary of the mean or variance will produce NAN's and that the maximum lag allowed for
#' any given parameter is 1. The default values are set as
#' list(omega = 5e-04, alpha = 0, gamma = NULL, beta = 0, mu = 0, ar = NULL, ma = NULL, delta = 2). In order
#' to set different parameters for each asset simply invert an vector of length equal to the number of assets,
#' the first element of the vector will corresping to Asset_1, the 2nd to Asset_2 ect...
#' See the "model" parameter under fGarch::garchSpec() for more details regarding the parameters themselves.
#' @return a tidy tibble containing a date, Asset and Return column.
#'
#' @importFrom copula P2p ellipCopula archmCopula rCopula
#' @importFrom glue glue
#' @importFrom sgt qsgt
#' @importFrom lubridate '%m+%' days
#' @importFrom tidyr gather
#' @import dplyr
#' @import purrr
#'
#' @examples
#'
#' \dontrun{
#'
#' library(tidyverse)
#'
#' ### creating a correlation matrix of 50 assets to use as an input in sim_asset_market.
#' corr <- gen_corr(N = 20, Clusters = "none")
#'
#' ### simulating 500 periods of returns across 50 assets.
#' set.seed(12345)
#' market_data <-
#'          sim_market(corr,
#'                      k = 500,
#'                      mv_dist = "norm",
#'                      left_cop_weight = 0.1,
#'                      marginal_dist = "norm",
#'                      ts_model = list(mu = 0.000002,
#'                                      omega = 0.00005,
#'                                      alpha = 0.098839,
#'                                      beta = 0.899506,
#'                                      ar = 0.063666,
#'                                      ma = NULL,
#'                                      gamma = 0.12194,
#'                                      delta = 1.85))
#'
#'  ### Visualising the market
#'  market_data %>%
#'  group_by(Asset) %>%
#'  mutate(cum_ret = 100*cumprod(1 + Return)) %>%
#'  ggplot() +
#'  geom_line(aes(x = date, y = cum_ret, color = Asset)) +
#'  facet_wrap(~Asset) +
#'  theme(legend.position = "none")
#'
#' }
#' @export

sim_market <- function(corr,
                       k = 252,
                       mv_dist = "t",
                       mv_df = 4,
                       left_cop_weight = 0,
                       left_cop_param = 5,
                       marginal_dist = "norm",
                       marginal_dist_model = NULL,
                       ts_model = NULL) {

    N <- nrow(corr)
    k <- k + 1   # extra room for sim_garch to as a lag.
    Cor <- P2p(corr)

    # Specifying  Copulas
    # elliptical
    if(!(mv_dist %in% c("norm", "t"))) stop("Please supply a valid argument for mv_dist")
    else
        if (mv_dist == "t") {
            if (is.null(mv_df)) stop('Please supply a valid degrees of freedom parameter when using mv_dist = "t".')
            Ecop <- ellipCopula(family = "t",
                                dispstr = "un",
                                df = mv_df,
                                param = Cor,
                                dim = N)
        } else
            if (mv_dist == "norm") {
                Ecop <- ellipCopula(family = "normal",
                                    dispstr = "un",
                                    param = Cor,
                                    dim = N)
            }

    # Left-cop (Archemedian copula)
    if (left_cop_weight < 0|left_cop_weight > 1) stop("Please provide a valid left_cop_weight between 0 and 1")
    if (left_cop_weight != 0) {
        Acop <- archmCopula(family = "clayton",
                            param = left_cop_param,
                            dim = N)
    }

    # Generating random (uniformly distributed) draws from hybrid copula's
    if (left_cop_weight == 0) {
        data <- rCopula(k, Ecop)
    } else
        if(left_cop_weight == 1) {
            data <- rCopula(k, Acop)
        } else {
            data <- (left_cop_weight*rCopula(k, Acop) + (1-left_cop_weight)*rCopula(k, Ecop))
        }


    # Creating a date vector
    start_date <- Sys.Date()
    dates <- rmsfuns::dateconverter(StartDate = start_date,
                                    EndDate = start_date %m+% lubridate::days(k-1),
                                    Transform = "alldays")

    # Making Tidy & adding date column
    data <- as_tibble(data) %>%
        purrr::set_names(glue::glue("Asset_{1:ncol(data)}")) %>%
        mutate(date = dates) %>%
        gather(Asset, Value, -date)


    if (!(marginal_dist %in% c("norm", "t", "sgt", "unif"))) stop ("Please supply a valid marginal_dist argument")

    if (marginal_dist == "unif") return(data)

    # Warnings
    if (marginal_dist == "norm" & is.null(marginal_dist_model)) marginal_dist_model <- list(mu=0, sd = 1)
    if (marginal_dist == "t" & is.null(marginal_dist_model))  marginal_dist_model <- list(mu=0, df = 5)
    if (marginal_dist == "sgt" & is.null(marginal_dist_model)) stop ('Please supply a valid marginal_dist_model when using marginal_dist="sgt".')

    #Converting Uniform marginal distributions to norm, t or sgt.
    args <- tibble(Asset = glue::glue("Asset_{1:N}")) %>%
        mutate(mean = marginal_dist_model$mu,
               sd = marginal_dist_model$sd,
               ncp = marginal_dist_model$ncp,
               df = marginal_dist_model$df,
               lambda = marginal_dist_model$lambda,
               p = marginal_dist_model$p,
               q = marginal_dist_model$q)

    if (marginal_dist == "norm") {

        if(is.null(marginal_dist_model$mu)) stop('Please supply a valid mu parameter when using marginal_dist = "norm".')
        if(is.null(marginal_dist_model$sd)) stop('Please supply a valid sd parameter when using marginal_dist = "norm".')

        data <- data %>% left_join(., args, by = "Asset") %>%
            group_by(Asset) %>%  arrange(date) %>%
            mutate(Return =  qnorm(Value, mean, sd)) %>%
            select(date, Asset, Return)

    } else
        if (marginal_dist == "t") {

            if(is.null(marginal_dist_model$ncp)) stop('Please supply a valid ncp parameter when using marginal_dist = "t".')
            if(is.null(marginal_dist_model$df)) stop('Please supply a valid df parameter when using marginal_dist = "t".')

            data <- data %>% left_join(., args, by = "Asset") %>%
                group_by(Asset) %>%  arrange(date) %>%
                mutate(Return = qt(Value, df =  df, ncp =  ncp)) %>%
                select(date, Asset, Return)

        } else
            if (marginal_dist == "sgt") {

                if (is.null(marginal_dist_model$mu)) marginal_dist_model$mu <- 0
                if (is.null(marginal_dist_model$sd)) marginal_dist_model$sd <- 1
                if (is.null(marginal_dist_model$lambda)|
                    is.null(marginal_dist_model$p)|
                    is.null(marginal_dist_model$q)) stop('Please supply valid arguments for lambda, p and q when using marginal_dist = "sgt".')

                data <- data %>% left_join(., args, by = "Asset") %>%
                    group_by(Asset) %>% arrange(date) %>%
                    mutate(Return = qsgt(Value, mean, sd, lambda, p, q)) %>%
                    select(date, Asset, Return)

            }

    if (is.null(ts_model)) {

        data <- data %>% dplyr::filter(date > first(date))
        return(data)

    } else {

        # Warnings
        if (!is.null(ts_model$omega) & length(ts_model$omega) != 1 & length(ts_model$omega) != N) stop("Please supply a valid vector length for omega. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$alpha) & length(ts_model$alpha) != 1 & length(ts_model$alpha) != N) stop("Please supply a valid vector length for alpha. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$gamma) & length(ts_model$gamma) != 1 & length(ts_model$gamma) != N) stop("Please supply a valid vector length for gamma. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$beta) & length(ts_model$beta) != 1 & length(ts_model$beta) != N) stop("Please supply a valid vector length for beta. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$mu) & length(ts_model$mu) != 1 & length(ts_model$mu) != N) stop("Please supply a valid vector length for mu. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$ar) & length(ts_model$ar) != 1 & length(ts_model$ar) != N) stop("Please supply a valid vector length for ar. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$ma) & length(ts_model$ma) != 1 & length(ts_model$ma) != N) stop("Please supply a valid vector length for ma. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$delta) & length(ts_model$delta) != 1 & length(ts_model$delta) != N) stop("Please supply a valid vector length for delta. Must be of length 1 or ncol(corr).")

        # Inducing mean and/or variance persistence

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
        data <- data %>% left_join(., ts_args, by = "Asset") %>%
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
        return(data)
    }
}
