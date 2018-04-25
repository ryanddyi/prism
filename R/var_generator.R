#' PRISM regressors generator
#'
#' Stage 1 of PRISM. The function generates prism seasonal components and seasonally adjusted lag components.
#'
#' @param data time series of interest as xts, last element can be NA.
#' @param data.early historical time series of response variable before Google Trend data is available. (e.g., unemployment initial claim prior to 2004)
#' @param stl if TRUE, use STL seasonal decomposition; if FALSE, use classic additive seasonal decomposition.
#' @param n.history training period for seasonal decomposition. (by default = 700 wks)
#' @param s.window seasonality span  (by default = 52 for weekly data)
#' @param n.lag the number of lags to be used as regressor in Stage 2 of PRISM (by default = 1:52 for weekly data)
#'
#' @return A list of following named objects
#' \itemize{
#' \item \code{y.lags} seasonally adjusted components, z_lag, and seasonal components, s_lag.
#' }
#'
#' @importFrom zoo index
#'
#' @export

var_generator<-function(data, data.early, stl = TRUE, n.lag = 1:52, s.window = 52, n.history = 700){

  n.past = length(data.early) # the length of all historical data
  data.comb = rbind(data.early,data)
  y.lags = array(NA, dim = c(length(data),2*length(n.lag)))

  for(i in 1:length(data)){
    y.vec = as.vector(data.comb[i+n.past-n.history:1])
    y.ts <- ts(y.vec, frequency = s.window)
    if (stl){
      # stl seasonal decomposition
      seasonal = stl(y.ts, s.window = s.window)
      y.trend = y.vec-seasonal$time.series[,1]
      y.lags[i,] = c(tail(y.trend,max(n.lag)), tail(seasonal$time.series[,1],max(n.lag)))
    } else {
      # classic additive decomposition
      decom = decompose(y.ts,  type ="additive")
      y.trend = y.vec-decom$seasonal
      y.lags[i,] = c(tail(y.trend,max(n.lag)), tail(decom$seasonal,max(n.lag)))
    }
  }
  colnames(y.lags) = c(paste0("z.lag_", sort(n.lag,decreasing = T)),
                       paste0("s.lag_", sort(n.lag,decreasing = T)))
  var_gen = list(y.lags=y.lags)
}

