#' @title sim_market
#' @description This function simulates a series of returns for an asset market with a wide range of
#' user-defined cross-section and time-series risk-return properties.
#' @note  It is suggested that if the ts_model argument is used, then the marginal distributions be
#' left as list(mu = 0, sd = 1) for marginal_dist = "norm" or "sgt", and list(ncp = 0) for marginal_dist = "t".
#'
#' If this is not done, a warning message will appear. It is better to set these attributes with the
#' ts_model argument, see the mu and omega parameters.
#' @param corr a correlation matrix specifying the correlation structure of the simulated data. The number
#' of variables simulated is equal to the number of columns/rows. When using mv_dist = "clayton", then simulated
#' returns do not adhere to this correlation matrix.
#' @param k a positive integer indicating the number of time periods to simulate.
#' @param mv_dist a string specifying the multivariate distribution. It can be one of c("norm", "t", "clayton"), these
#' correspond to the respective multivariate normal, t and Clayton distributions. This will effect how the
#' returns are cross-sectionally distributed. When using mv_dist = "clayton", assets will exhibit high left-tail dependence,
#' but will not adhere to the user-defined correlation matrix.
#' @param mv_df degrees of freedom of the multivariate t distribution (> 0, can be a non-integer).
#'
#' The default is 4 and is only needed when mv_dist = "t".
#' @param clayton_param a value (> 0, can be a non-integer) indicating the parameter of the Clayton copula.
#'
#' The default is 1 and is only needed when mv_dist = "clayton".
#' @param marginal_dist a string variable specifying the univariate distribution of the asset return series. This can
#' be one of c("norm", "t", "sgt") referring to the normal, student-t and skewed-generalized-t distributions, respectively.
#' Default is "norm".
#' @param  marginal_dist_model list containing the relevant parameters for the chosen marginal_dist_ model.
#'
#' marginal_dist = "norm" accepts the mean (mu) and standard deviation (sd) arguments with their respective
#' defaults set to list(mu = 0, sd = 1).
#'
#' marginal_dist = "t" accepts the non-centrality parameter (ncp) and degrees of freedom (df) arguments,
#' default values are list(ncp = 0, df = 5).
#'
#' marginal_dist = "sgt" accepts the mean (mu), standard deviation (sd), lambda, p and q parameters
#' list(mu = 0, sigma = 1, lambda, p, q). Note that lambda, p and q have no defaults and must, therefore, be set
#' by the user. For more information on the parameters see ?sgt::sgt.
#' @param ts_model a list containing various ARMA + APGARCH model parameters. These parameters specify
#' the time-series properties of the simulated returns. Note that parameter combinations resulting
#' in non-stationarity of the mean or variance will produce NAN's and that the maximum lag allowed for
#' any given parameter is 1.
#'
#' The default is ts_model = NULL, in which case the time-series properties are not induced, however, if
#' ts_model = list() then the default values are list(omega = 5e-04, alpha = 0, gamma = NULL, beta = 0, mu = 0,
#' ar = NULL, ma = NULL, delta = 2). In order to set different parameters for each asset, simply insert a vector
#' of length equal to the number of assets, the 1st element of the vector will correspond to Asset_1, the 2nd
#' to Asset_2 ect...
#'
#' For more details on the ARMA + APGARCH, see the ?sim_garch and the "model" parameter in ?fGarch::garchSpec.
#' @param progress a logical value indicating if sim_market should produce a progress bar when iterated over. See
#' examples on how to use correctly.
#'
#' Due to memory concerns, when simulating many markets it is suggested that users use map() over map_dfr().
#' map_dfr() produces a long/tidy data set and is therefore, useful when wanting to produce a plot with ggplot2.
#'
#'
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
#' library(MCmarket)
#'
#' #=============================
#' # Simulating a single market
#' #=============================
#' ### creating a correlation matrix of 50 assets to use as an input in sim_asset_market.
#' corr <- gen_corr(D = 20, clusters = "none")
#'
#' ### simulating 500 periods of returns across 50 assets.
#' set.seed(12345)
#' market_data <-
#'          sim_market(corr,
#'                      k = 500,
#'                      mv_dist = "norm",
#'                      marginal_dist = "norm",
#'                      ts_model = list(mu = 0.000002,
#'                                      omega = 0.00005,
#'                                      alpha = 0.09,
#'                                      beta = 0.8,
#'                                      ar = 0.06,
#'                                      ma = NULL,
#'                                      gamma = 0.001,
#'                                      delta = 1.85),
#'                      progress = FALSE)
#'
#'  ### Visualising the market
#'  market_data %>%
#'  group_by(Asset) %>%
#'  mutate(cum_ret = 100*cumprod(1 + Return)) %>%
#'  ggplot() +
#'  geom_line(aes(x = date, y = cum_ret, color = Asset)) +
#'  facet_wrap(~Asset, scales = "free_y") +
#'  theme(legend.position = "none")
#'
#' #==================================================
#' # Performing Monte Carlo's with a Progress bar
#' #==================================================
#'
#' ### For small N<500, can use map_dfr for tidy output.
#' N <- 50
#' pb <- dplyr::progress_estimated(N) # Setting length of progress bar, Must be named pb.
#' market <-
#'       map_dfr(1:N,
#'               ~sim_market(corr,
#'                           marginal_dist = "norm",
#'                           marginal_dist_model = list(mu = 0.02, sd = 0.5),
#'                           progress = TRUE),
#'               .id = "Universe") # adds an extra key/identification column.
#'
#' ### Visualizing the market
#'  market %>% group_by(Asset, Universe) %>%
#'  mutate(cum_ret = 100*cumprod(1 + Return)) %>%
#'          ggplot() +
#'          geom_line(aes(x = date, y = cum_ret, color = Universe)) +
#          facet_wrap(~Asset, scales = "free_y") +
#'         theme(legend.position = "none")
#'
#' ### For large N>500, should rather use map for list output.
#' N <- 1000
#' pb <- dplyr::progress_estimated(N)   # this must be named pb
#' market <- map(1:N,
#'               ~sim_market(corr, progress = TRUE))
#'
#' }
#' @export

sim_market <- function(corr,
                       k = 252,
                       mv_dist = "t",
                       mv_df = 3,
                       clayton_param = 1,
                       marginal_dist = "norm",
                       marginal_dist_model = NULL,
                       ts_model = NULL,
                       progress = FALSE) {
    if (progress == TRUE)
        pb$tick()$print()

    N <- nrow(corr)
    k <- k + 1   # extra room for sim_garch to as a lag.
    Cor <- P2p(corr)

    # Specifying  Copulas: 0.5 MB
    # elliptical
    if (!(mv_dist %in% c("norm", "t", "clayton")))
        stop("Please supply a valid argument for mv_dist")
    if (mv_dist == "t") {
        if (is.null(mv_df))
            stop('Please supply a valid degrees of freedom parameter when using mv_dist = "t".')
        cop <- ellipCopula(
            family = "t",
            dispstr = "un",
            df = mv_df,
            param = Cor,
            dim = N
        )
    } else
        if (mv_dist == "norm") {
            cop <- ellipCopula(
                family = "normal",
                dispstr = "un",
                param = Cor,
                dim = N
            )
        } else
            if (mv_dist == "clayton") {
                cop <- archmCopula(family = "clayton",
                                   param = clayton_param,
                                   dim = N)
            }

    # Generating random (uniformly distributed) draws from hybrid copula's
    #data <- rCopula(k, cop)

    # Creating a date vector
    start_date <- Sys.Date()
    dates <- rmsfuns::dateconverter(
        StartDate = start_date,
        EndDate = start_date %m+% lubridate::days(k -
                                                      1),
        Transform = "alldays"
    )

    # Generating random (uniformly distributed) draws from hybrid copula's
    # and Making Tidy + adding date column
    data <- as_tibble(rCopula(k, cop)) %>%
        purrr::set_names(glue::glue("Asset_{1:N}")) %>%
        mutate(date = dates) %>%
        gather(Asset, Value,-date)


    if (!(marginal_dist %in% c("norm", "t", "sgt", "unif")))
        stop ("Please supply a valid marginal_dist argument")

    if (marginal_dist == "unif") {
        return(data)
    }

    # Warnings
    if (marginal_dist == "norm" & is.null(marginal_dist_model)) marginal_dist_model <- list(mu=0, sd = 1)
    if (marginal_dist == "t" & is.null(marginal_dist_model))  marginal_dist_model <- list(ncp = 0, df = 5)
    if (marginal_dist == "sgt" &
        is.null(marginal_dist_model))
        stop ('Please supply a valid marginal_dist_model when using marginal_dist="sgt".')

    # Converting Uniform marginal distributions to norm, t or sgt.
    args <- tibble(Asset = glue::glue("Asset_{1:N}")) %>%
        mutate(mean = marginal_dist_model$mu,
               sd = marginal_dist_model$sd,
               ncp = marginal_dist_model$ncp,
               df = marginal_dist_model$df,
               lambda = marginal_dist_model$lambda,
               p = marginal_dist_model$p,
               q = marginal_dist_model$q)

    if (marginal_dist == "norm") {

        if (is.null(marginal_dist_model$mu))
            stop('Please supply a valid mu parameter when using marginal_dist = "norm".')
        if (is.null(marginal_dist_model$sd))
            stop('Please supply a valid sd parameter when using marginal_dist = "norm".')
        if (any((marginal_dist_model$mu != 0) |
            any(marginal_dist_model$sd != 1)) &
            !(is.null(ts_model)))
            warning(
                "you have double set the mean and sd moments: to fix set marginal_dist_model = list(mu = 0, sd = 1). These moments are better set in ts_model, see mu and omega"
            )

        data <- data %>% left_join(., args, by = "Asset") %>%
            group_by(Asset) %>%  arrange(date) %>%
            mutate(Return =  qnorm(Value, mean, sd)) %>%
            ungroup() %>% select(date, Asset, Return)

    } else
        if (marginal_dist == "t") {
            if (is.null(marginal_dist_model$ncp))
                stop('Please supply a valid ncp parameter when using marginal_dist = "t".')
            if (is.null(marginal_dist_model$df))
                stop('Please supply a valid df parameter when using marginal_dist = "t".')
            if (any(marginal_dist_model$ncp != 0) &
                !(is.null(ts_model)))
                warning(
                    "you have double set moments: to fix set marginal_dist_model = list(ncp = 0) and then change intercept term using ts_model = list(mu)"
                )

            data <- data %>% left_join(., args, by = "Asset") %>%
                group_by(Asset) %>%  arrange(date) %>%
                mutate(Return = qt(p = Value, df =  df, ncp =  ncp)) %>%
                ungroup() %>% select(date, Asset, Return)

        } else
            if (marginal_dist == "sgt") {
                if (is.null(marginal_dist_model$mu))
                    marginal_dist_model$mu <- 0
                if (is.null(marginal_dist_model$sd))
                    marginal_dist_model$sd <- 1
                if (is.null(marginal_dist_model$lambda) |
                    is.null(marginal_dist_model$p) |
                    is.null(marginal_dist_model$q))
                    stop('Please supply valid arguments for lambda, p and q when using marginal_dist = "sgt".')
                if ((any(marginal_dist_model$mu != 0) |
                     any(marginal_dist_model$sd != 1)) &
                    !(is.null(ts_model)))
                    warning(
                        "you have double set the mean and sd moments: to fix set marginal_dist_model = list(mu = 0, sd = 1). These moments are better set in ts_model, see mu and omega"
                    )

                data <-
                    data %>% left_join(., args, by = "Asset") %>%
                    group_by(Asset) %>% arrange(date) %>%
                    mutate(Return = qsgt(Value, mean, sd, lambda, p, q)) %>%
                    ungroup() %>% select(date, Asset, Return)

            }

    if (is.null(ts_model)) {
        data <- data %>% dplyr::filter(date > first(date))
        return(data)

    } else {
        # Warnings
        if (!is.null(ts_model$omega) &
            length(ts_model$omega) != 1 &
            length(ts_model$omega) != N
        )
            stop("Please supply a valid vector length for omega. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$alpha) &
            length(ts_model$alpha) != 1 &
            length(ts_model$alpha) != N)
            stop("Please supply a valid vector length for alpha. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$gamma) &
            length(ts_model$gamma) != 1 &
            length(ts_model$gamma) != N)
            stop("Please supply a valid vector length for gamma. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$beta) &
            length(ts_model$beta) != 1 &
            length(ts_model$beta) != N)
            stop("Please supply a valid vector length for beta. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$mu) &
            length(ts_model$mu) != 1 &
            length(ts_model$mu) != N)
            stop("Please supply a valid vector length for mu. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$ar) &
            length(ts_model$ar) != 1 &
            length(ts_model$ar) != N)
            stop("Please supply a valid vector length for ar. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$ma) &
            length(ts_model$ma) != 1 &
            length(ts_model$ma) != N)
            stop("Please supply a valid vector length for ma. Must be of length 1 or ncol(corr).")
        if (!is.null(ts_model$delta) &
            length(ts_model$delta) != 1 &
            length(ts_model$delta) != N)
            stop("Please supply a valid vector length for delta. Must be of length 1 or ncol(corr).")

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
            mutate(
                Return = sim_garch(
                    innovations = Return,
                    omega = omega,
                    alpha = alpha,
                    gamma = gamma,
                    beta = beta,
                    mu = mu,
                    ar = ar,
                    ma = ma,
                    delta = delta
                )
            ) %>% na.omit() %>%
            ungroup() %>% select(date, Asset, Return)
        return(data)
    }


}
