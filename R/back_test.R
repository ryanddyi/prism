#' Out-of-sample prediction for whole period
#'
#' @param n.history length of training period (e.g. in weeks) for seasonal decomposition.
#' @param s.window seasonality span in seasonal decomposition (by default = 52 for weekly data)
#' @param n.lag the number of lags to be used as regressor in Stage 2 of PRISM (by default = 1:52 for weekly data)
#' @param stl if TRUE, use STL seasonal decomposition; if FALSE, use classic additive seasonal decomposition.
#' @param n.training length of training period in Stage 2, penalized linear regression (by default = 156)
#' @param alpha penalty between lasso and ridge. alpha=1 represents lasso, alpha=0 represents ridge, alpha=NA represents no penalty (by default alpha = 1).
#' @param nPred the number of periods ahead for forecast. nPred = 0,1,2,3.
#' @param UseGoogle boolean variable indicating whether to use Google Trend data.
#' @param discount exponential weighting: (1-discount)^lag.
#' @param sepL1 if TRUE, use separate L1 regularization parameters for time series components and exogenous variables (Goolgle Trend data)
#' @return \code{prediction} \code{nPred} week ahead prediction of the whole periods (07 - 20).
#'
#' @examples
#' claim_data = load_claim_data()
#'
#' \donttest{# It may take a few minutes.}
#' \donttest{prism_prediction = back_test()}
#' \donttest{# evaluate the out-of-sample prediction error as a ratio to naive method}
#' \donttest{evaluation_table(claim_data, prism_prediction)}
#'
#' @export


back_test<-function(n.lag=1:52, s.window=52, n.history=700, stl = TRUE, n.training=156,
                    UseGoogle=T, alpha=1, nPred=0, discount = 0.01, sepL1 = F){

  folders_5y = list.files(system.file("extdata/search_data_5year", package = "PRISM.forecast"))
  prism.fit = list()

  for (folder in folders_5y){

    # load search data & claim data
    prism_data = load_5y_search_data(folder)

    # seasonal decomposition

    var.seasonal = var_generator(data = prism_data$claim.data, data.early = prism_data$claim.earlyData,
                                 stl = T, n.lag=n.lag, s.window = s.window, n.history = n.history)

    prism.fit[[folder]] = prism_batch(data = prism_data$claim.data,
                              GTdata = prism_data$allSearch, var = var.seasonal,
                              n.training = n.training, nPred.vec = c(nPred), UseGoogle = UseGoogle,
                              alpha = alpha, discount = discount, sepL1 = sepL1)

    if(folder == '0408'){
      prediction.all = prism.fit[[folder]]$pred
    } else {
      prediction.all = rbind(prediction.all, prism.fit[[folder]]$pred)
    }
  }

  prediction.all = prediction.all[,nPred+1]
  prediction = prediction.all[-which(stats::time(prediction.all)[-1]==stats::time(prediction.all)[-length(prediction.all)])]
  list(prediction = prediction, nPred = nPred)
}
