#' @title mc_market
#' @description This function performs a Monte Carlo simulation by iterating over the the sim_market function N times.
#' It is intended for users who are not comfortable using the purrr::map functions.
#' @note (1) see ?sim_market for information on the other parameters.
#'
#' (2) See examples under sim_market for instructions on how to add an on-screen progress bar when performing
#' the Monte Carlo simulation, this is recommended for simulations with N >1000 since they can take a number of
#' minuets to complete.
#' @param N a positive integer indicating the number of markets to simulate.
#'
#' @param list a logical value indicating whether the output should be a list of tibbles or a single long tibble (see return).
#' Due to memory issues associated with list = FALSE, list = TRUE is recommended for N > 500. List = FALSE is
#' best used for tidy output that can easily be plotted with ggplot2 (see example).
#'
#' @return if list = TRUE (default), a list of length N where each entry contains a tidy tibble with a date,
#' Asset and Return column. Else if list = FALSE a single tidy tibble with date, Universe, Asset and Return columns.
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
                      clayton_param = 4,
                      marginal_dist = "norm",
                      marginal_dist_model = NULL,
                      ts_model = NULL,
                      list = TRUE) {
    if (list == TRUE) {

        pb <- dplyr::progress_estimated(N) # setting length of progress bar

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
