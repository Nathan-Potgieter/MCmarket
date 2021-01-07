#' @title sim_market_with_progress
#' @description This is an alternative version of the sim_market function that
#' includes functionality to include a progress bar when used in purrr::map functions
#' @note  See ??sim_market for details on use.
#' @return a tidy tibble containing a date, Asset and Return column.
#'
#' @importFrom copula ellipCopula archmCopula rcopula
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
#' library(MCmarket)
#' library(tidyverse)
#'
#' ### creating a correlation matrix to use as input in sim_asset_market
#' corr <- gen_corr(N = 20, Clusters = "none")
#'
#'
#' N <- 100
#' pb <- dplyr::progress_estimated(N)   # this must be named pb
#' market <-
#'       map_dfr(1:N,
#'               ~sim_market_with_progress(corr),
#'               .id = "Universe")
#'
#' }
#' @export

sim_market_with_progress <- function(corr,
                                     k = 252,
                                     mv_dist = "t",
                                     mv_df = 3,
                                     left_cop_weight = 0,
                                     left_cop_param = 4,
                                     marginal_dist = "norm",
                                     marginal_dist_model = NULL,
                                     ts_model = list()
) {
    pb$tick()$print()

    #Simulating innovations
    inno <- sim_inno(corr = corr,
                     mv_dist = mv_dist,
                     mv_df = mv_df,
                     left_cop_param = left_cop_param,
                     left_cop_weight = left_cop_weight,
                     marginal_dist = marginal_dist,
                     marginal_dist_model = marginal_dist_model,
                     k = k)

    #creating a date vector
    start_date <- Sys.Date()
    dates <- rmsfuns::dateconverter(StartDate = start_date,
                                    EndDate = start_date %m+% lubridate::days(k-1),
                                    Transform = "alldays")

    if (is.null(ts_model)) {
        return(
            inno[6:nrow(inno),] %>% mutate(date = dates, .before = `Asset_1`) %>%
                gather(key = Asset, value = Return, -date)
        )
    } else

        #Applying sim_garch to each column in simdat

        simdat <- inno %>% map_dfc(~sim_garch(innovations = .x, model =  ts_model))

    #Creating final df
    simdat %>%
        mutate(date = dates, .before = `Asset_1`) %>%
        gather(key = Asset, value = Return, -date)
}
