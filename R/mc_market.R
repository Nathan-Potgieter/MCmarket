#' @title mc_market
#' @description Repeatedly reruns the the sim_market function N number of times. It is there intended for
#' users who are not comfortable using the purrr::map functions.
#' @note See examples under sim_market_with_progress for instructions on how to add an on screen
#'  progress bar when performing the Monte Carlo simulation, this recommended for simulations with N >1000
#'  as they can take a number of minuets.
#'
#' It is suggested that, if the ts_model argument is used, then the marginal distributions be left
#' as list(mu = 0, sd = 1). These attributes are better off being set in the ts_model argument by the
#' mu and omega parameters respectively
#' @param corr a correlation matrix specifying the correlation structure of the simulated data. The number
#' of variables simulated is equal to the number of columns/rows in the correlation matrix. When using
#' mv_dist = "clayton" only the dimensions of the correlation matrix and hence correlations will not adhere to
#' the inputted corr matrix.
#' @param N a positive integer indicating the number of markets to simulate.
#' @param k a positive integer indicating the number of time periods to simulate.
#' @param mv_dist a string specifying the multivariate distribution. Can be one of c("norm", "t", "clayton")
#' which correspond to the multivariate normal, t and Clayton distribution respectively.
#' @param mv_df degrees of freedom of the multivariate t distribution, only used when mv_dist = "t". Default is 3.
#' @param clayton_param a positive value indicating the Clayton copula's parameter. Default is 4.
#' @param marginal_dist a string variable specifying the univariate distribution of the asset return series. Can
#' be one of c("norm", "t", "sgt") referring to the normal, student-t and skewed-generalized-t
#' distributions respectively. Default is "norm".
#' @param  marginal_dist_model list containing the relevant parameters for the chosen marginal_dist.
#'
#' marginal_dist = "norm" accepts the mean (mu) and standard deviation (sd) arguments with their respective
#' defaults set to list(mu = 0, sd = 1).
#'
#' marginal_dist = "t" accepts the non-centrality parameter (ncp) and degrees of freedom (df) arguments,
#' default values are list(ncp = 0, df = 5).
#'
#' marginal_dist = "sgt" accepts the mean (mu), standard deviation (sd), lambda, p and q parameters
#' list(mu = 0, sigma = 1, lambda, p, q). Note lambda, p and q have no defaults and must therefore be set
#' by the user. For more information see the documentation on the qsgt function from the sgt pacakge.
#' @param ts_model a list containing various ARMA + APGARCH model parameters allowing one to specify
#' the time series properties of the simulated returns. Note that parameter combinations resulting
#' in non-stationary of the mean or variance will produce NAN's and that the maximum lag allowed for
#' any given parameter is 1.
#'
#' The default is ts_model = NULL, in which case time series time series properties are not induced, however if
#' ts_model = list() then the default values are list(omega = 5e-04, alpha = 0, gamma = NULL, beta = 0, mu = 0,
#' ar = NULL, ma = NULL, delta = 2). In order to set different parameters for each asset simply insert a vector
#' of length equal to the number of assets, the first element of the vector will correspond to Asset_1, the 2nd
#' to Asset_2 ect...
#'
#' See the sim_garch function's documentation and the "model" parameter under fGarch::garchSpec() for more details
#' regarding the parameters themselves.
#'
#'@param list a logical value indicating whether the output should be a list of tibbles or a single long tibble (see return).
#' Due to memory issues associated with list = FALSE, it is recomended to use list = TRUE for N > 1000. List  == FALSE is
#' best used when wanting to easily plot output using ggplot2 (see example).
#'
#' @param output
#'
#' @return if list = TRUE (default), a list of lenghth N with each entery containing a tidy tibble containing with a  date,
#' Asset and Return column. Else if list = FALSE a single tidy tibble with a date, Universe, Asset and Return column.
#'
#'
#' @importFrom tidyr gather
#' @importFrom dplyr progress_estimated
#' @import dplyr
#' @import purrr
#'
#' @examples
#'
#' \dontrun{
#'
#' library(tidyverse)
#'
#' ### creating a correlation matrix to use as input in sim_asset_market
#' corr <- gen_corr(D = 20, clusters = "none")
#'
#' ### simulating 550 periods of returns across 50 assets and 100 universes
#' set.seed(12542)
#' market_data <- sim_asset_market(corr,
#'                                 k = 550,
#'                                 mv_dist = "norm",
#'                                 marginal_dist = "norm",
#'                                 ts_model = list(mu = 0.000002,
#'                                                 omega = 0.00005,
#'                                                 alpha = 0.098839,
#'                                                 beta = 0.899506,
#'                                                 ar = 0.063666,
#'                                                 ma = NULL,
#'                                                 gamma = 0.12194,
#'                                                 delta = 1.85))
#'
#'  ### Visualizing the market
#'  market_data %>% group_by(Asset) %>%
#'  mutate(cum_ret = 100*cumprod(1 + Return)) %>%
#'          ggplot() +
#'          geom_line(aes(x = date, y = cum_ret, color = Asset)) +
#'          facet_wrap(~Asset) +
#'          theme(legend.position = "none")
#'
#' }
#' @export
mc_market <- function(corr,
                      N = 100,
                      k = 252,
                      mv_dist = "t",
                      mv_df = 3,
                      left_cop_weight = 0,
                      left_cop_param = 4,
                      marginal_dist = "norm",
                      marginal_dist_model = NULL,
                      ts_model = list(),
                      list = TRUE) {
    if (list == TRUE) {
        1:N %>%
            map(
                ~ sim_market(
                    corr = corr,
                    k = k,
                    mv_dist = mv_dist,
                    mv_df = mv_df,
                    clayton_param = clayton_param,
                    marginal_dist = marginal_dist,
                    marginal_dist_model = marginal_dist_model,
                    ts_model = ts_model
                )
            )

    } else
        if (list == FALSE) {
            1:N %>%
                map_dfr(
                    ~ sim_market(
                        corr = corr,
                        k = k,
                        mv_dist = mv_dist,
                        mv_df = mv_df,
                        clayton_param = clayton_param,
                        marginal_dist = marginal_dist,
                        marginal_dist_model = marginal_dist_model,
                        ts_model = ts_model
                    ),
                    .id = "Universe"
                )
        }

}
