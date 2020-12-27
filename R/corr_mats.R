#' @title corr_mats
#' @description This is a data set containing 30 empirical correlation matrices
#' obtained by randomly selecting a set of 50 stocks from the S&P 500 over a random
#' time period of 252 days between 1 January 2000 and 26 December 2020. The correlation
#' matrices were then labelled as ‘stressed’ if the equi-weighted basket of stocks had
#' a Sharpe ratio below -0.5, ‘rally’ if the equi-weighted basket of stocks had a Sharpe
#' ratio above 2 over and ‘normal’ if the equi-weighted basket of stocks had a Sharpe
#' in-between -0.5 and 2.
load("corr_mats.Rda")
