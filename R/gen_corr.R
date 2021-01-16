#' @title gen_corr
#' @description This function allows users to easily generate ad hoc correlation
#' matrices with a set number of clusters and up to 4 layers.
#' @param D The number of variables, generates an D by D correlation matrix.
#' @param clusters a character string specifying the type of cluster structure.
#' Available options are "none", for a correlation matrix with no clusters,
#' "non-overlapping" for a correlation matrix with one layer of clusters, and
#' "overlapping" for a correlation matrix with up to 4 layers and a set number
#' of clusters per layer.
#' @param num_clusters If clusters = "none" then num_clusters is not used.
#'
#' If clusters = "non-overlapping" then num_clusters is an integer indicating the number of clusters.
#'
#' If clusters = "overlapping" then num_clusters must be a vector of length less than or equal to 4. The length of
#' num_clusters specifies the number of  cluster layers and the integers within the vector specify the number of clusters
#' per layer. It is preferable to arranged the vector in descending order, but failing to do so can result in
#' unique output but may not contain the intended number of layers.
#' @return this function returns a D by D correlation matrix.
#'
#' @import propagate
#'
#' @examples
#' \dontrun{
#' library(ggcorrplot)
#'
#' ### This generates a 50 by 50 correlation matrix with no clusters.
#' gen_corr(D = 50, clusters = "none) %>%
#'          ggcorrplot(title = "no clusters")
#'
#' ### This generates a 50 by 50 correlation matrix with 5 non-overlapping clusters.
#' gen_corr(D = 50, clusters = "non-overlapping) %>%
#'          ggcorrplot(title = "non-overlapping clusters")
#'
#' ### This generates a 60 by 60 correlation matrix consisting
#' ### of 4 layers with 10, 5, 3 and 2 clusters respectively.
#' gen_corr(D = 60,
#'          clusters = "overlapping",
#'          num_clusters = c(10,5,3,2)) %>%
#'                   ggcorrplot(title = "overlapping clusters")
#'
#' ### Try tinkering with the num_clusters argument in unique ways to effect the
#' ### within cluster correlation coefficients.
#' ### Two cluster layers with low overall correlation.
#' gen_corr(D = 50,
#'          clusters = "overlapping",
#'          num_clusters = c(10,10,10,2)) %>%
#'                   ggcorrplot(title = "overlapping clusters") %>%
#'                   ggcorrplot(title = "overlapping clusters")
#'
#' }
#' @export
gen_corr <- function (D = 50,
                      clusters = c("none", "non-overlapping", "overlapping"),
                      num_clusters = NULL) {

        Grps <- num_clusters
        num_layers <- length(num_clusters)
        #set.seed(123)

        if (!(clusters %in%  c("none", "non-overlapping", "overlapping"))) stop("Please provide a valid clusters argument")

        if(clusters == "none"){
                # Unclustered covariance matrix
                Sigma <- diag(D)
                for (i in 1:D) for (j in 1:D) Sigma[i,j] <- 0.9^abs(i-j)
                Sigma <- propagate::cor2cov(Sigma, runif(D, 1, 5))
                corr <- cov2cor(Sigma)
        } else

                if(clusters == "non-overlapping"){
                        #----------------------
                        # distinct non-overlapping clusters:
                        #----------------------

                        if(is.null(num_clusters)) stop("Please provide a valid num_clusters argument when using non-overlapping clusters")
                        if (num_layers>1) stop("Please provede a valid num_clusters argument of length one when using non-overlapping clusters")

                        Sigma <- matrix(0.6, D, D)
                        diag(Sigma) <- 1


                        for (i in 1:Grps) {
                                ix <- seq((i-1) * D / Grps + 1, i * D / Grps)
                                Sigma[ix, -ix] <- 0.0001                       #think about
                        }
                        Sigma <- propagate::cor2cov(Sigma, runif(D, 1, 5))
                        corr <- cov2cor(Sigma)
                } else

                        if(clusters == "overlapping"){
                                #----------------------
                                # distinct overlapping clusters:
                                #----------------------
                                if(is.null(num_clusters)) stop("Please provide a valid num_clusters argument when using overlapping clusters")
                                if (num_layers>4) stop("Please provede a valid num_clusters argument of length less than or equal to 4 when using non-overlapping clusters")

                                Sigma <- matrix(0.7, D, D)
                                diag(Sigma) <- 1

                                for (i in 1:Grps[1]) {
                                        ix <- seq((i-1) * D / Grps[1] + 1, i * D / Grps[1])
                                        Sigma[ix, -ix] <- 0.5
                                }
                                if(num_layers>=2){
                                        for (i in 1:Grps[2]) {
                                                ix <- seq((i-1) * D / Grps[2] + 1, i * D / Grps[2])
                                                Sigma[ix, -ix] <- 0.3
                                        } }
                                if(num_layers>=3){
                                        for (i in 1:Grps[3]) {
                                                ix <- seq((i-1) * D / Grps[3] + 1, i * D / Grps[3])
                                                Sigma[ix, -ix] <- 0.15
                                        } }
                                if(num_layers>=4){
                                        for (i in 1:Grps[4]) {
                                                ix <- seq((i-1) * D / Grps[4] + 1, i * D / Grps[4])
                                                Sigma[ix, -ix] <- 0.05
                                        } }
                        }

        Sigma <- propagate::cor2cov(Sigma, runif(D, 1, 5))  #Is this necessary???
        corr <- cov2cor(Sigma)

        return(corr)

}
