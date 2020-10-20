#' PRISM function
#'
#' A function for nowcasting and forecasting time series.
#'
#' @param data time series of interest as xts, last element can be NA. (e.g., unemployment initial claim data in the same period as \code{GTdata}).
#' @param GTdata contemporaneous exogenous data as xts. (e.g., Google Trend data)
#' @param data.early historical time series of response variable before contemporaneous exogenous data, \code{GTdata} is available. (e.g., unemployment initial claim prior to 2004)
#' @param stl if TRUE, use STL seasonal decomposition; if FALSE, use classic additive seasonal decomposition.
#' @param n.history training period for seasonal decomposition. (by default = 700 wks)
#' @param n.training length of regression training period (by default = 156)
#' @param alpha penalty between lasso and ridge. alpha=1 represents lasso, alpha=0 represents ridge, alpha=NA represents no penalty.
#' @param nPred.vec the number of periods ahead for forecast. nPred.vec could be a vector of intergers. e.g. nPred.vec=0:3 gives results from nowcast to 3-week ahead forecast.
#' @param UseGoogle boolean variable indicating whether to use Google Trend data.
#' @param discount exponential weighting: (1-discount)^lag (by default = 0.015).
#' @param sepL1 if TRUE, use separate L1 regularization parameters for time series components and exogenous variables (Goolgle Trend data)
#'
#' @return A list of following named objects
#' \itemize{
#' \item \code{coef} coefficients for Intercept, z.lags, seasonal.lags and exogenous variables.
#' \item \code{pred} a vector of prediction with \code{nPred.vec} weeks forward.
#' }
#'
#' @importFrom zoo index
#'
#' @importFrom stats time coef predict lm sd
#'
#' @examples
#' prism_data = load_5y_search_data('0610')
#' data = prism_data$claim.data[1:200] # load claim data from 2006-01-07 to 2009-10-31
#' data[200] = NA # delete the data for the latest date and try to nowcast it.
#'
#' data.early = prism_data$claim.earlyData # load claim prior to 2006
#' GTdata = prism_data$allSearch[1:200] # load Google trend data from 2006-01-07 to 2009-10-31
#'
#' result = prism(data, data.early, GTdata) # call prism method
#' result$pred # output 0-3wk forward prediction
#'
#' @export


prism<-function(data, data.early, GTdata, stl = TRUE, n.history = 700, n.training = 156, alpha = 1,
                UseGoogle = T, nPred.vec=0:3, discount = 0.015, sepL1 = F){

  var = var_generator(data = data, data.early = data.early, stl)

  start.id = end.id = length(data)

  # n.lags determined by var
  n.lag = 1:dim(var$y.lags)[2]

  lasso.pred<-array(dim=c(length(data),max(nPred.vec+1)))

  colnames(lasso.pred) = paste0('forecast_',0:max(nPred.vec))

  lasso.coef<-list()

  for(nPred in nPred.vec){
    if (length(GTdata) > 0 & UseGoogle){
      lasso.coef[[nPred+1]] = array(dim=c(length(data),dim(GTdata)[2]+length(n.lag)+1))
    } else {
      lasso.coef[[nPred+1]] = array(dim=c(length(data),length(n.lag)+1))
    }
    rownames(lasso.coef[[nPred+1]]) = as.character(time(data))
  }

  for(nPred in nPred.vec){
    if (length(GTdata) > 0 & UseGoogle){
      if (!all(zoo::index(data) == zoo::index(GTdata)))
        stop("error in data and GTdata: their time steps must match")

      penalty.factor = c(rep(1,max(n.lag)), rep(1,dim(GTdata)[2]))
      design.matrix.all = cbind(var$y.lags[,n.lag], data.matrix(GTdata))
    } else {
      penalty.factor = rep(1,max(n.lag))
      design.matrix.all = var$y.lags[,n.lag]
    }

    # date[current+1] is the date
    for(current in start.id:end.id-1){

      training.idx <- (current - n.training + nPred + 1):current

      y.response <- data[training.idx]
      design.matrix <- scale(design.matrix.all[training.idx-nPred,])

      # standardize covariates for data available on date[current+1]
      newx <- matrix((design.matrix.all[current+1,]-colMeans(design.matrix.all[training.idx-nPred,]))/apply(design.matrix.all[training.idx-nPred,],2,sd),nrow=1)

      weights = (1-discount)^(length(training.idx):1)

      if (sepL1){
        # cross-validate at least one more tuning parameters
        # also output prediction result

        candidate_set = exp(-20:20/2)

        all.cvm = list()
        all.cvup = list()
        all.lambda = list()
        all.nzero = list()
        all.t.param = list()

        for(i in 1:length(candidate_set)){
          t.param = candidate_set[i] # the extra tuning parameter
          p.ratio = t.param

          penalty.factor=c(rep(p.ratio, max(n.lag)), rep(1,dim(GTdata)[2]))

          lasso.fit <- glmnet::cv.glmnet(x = design.matrix,
                                         y = y.response, weights = weights, nfolds = 10, grouped = FALSE,
                                         alpha = alpha, penalty.factor = penalty.factor)

          all.cvm[[i]] = lasso.fit$cvm
          all.cvup[[i]] = lasso.fit$cvup
          all.lambda[[i]] = lasso.fit$lambda
          all.nzero[[i]] = lasso.fit$nzero
          all.t.param[[i]] = rep(t.param, length(lasso.fit$cvm))
        }

        cvm.vec = unlist(all.cvm)
        cvup.vec = unlist(all.cvup)
        lambda.vec = unlist(all.lambda)
        nzero.vec = unlist(all.nzero)
        t.param.vec = unlist(all.t.param)

        cv2.up = cvup.vec[which.min(cvm.vec)]

        # all index such that cvm < cvup of the optim
        qualified = which(cvm.vec<cvup.vec[which.min(cvm.vec)])
        # choose the most sparse model in those qualified parameter settings
        id = qualified[which.min(nzero.vec[cvm.vec<cvup.vec[which.min(cvm.vec)]])]

        # reset the hyper-parameters and produce prediction
        t.param = t.param.vec[id]
        lambda = lambda.vec[id]
        p.ratio = t.param

        penalty.factor=c(rep(p.ratio, max(n.lag)), rep(1,dim(GTdata)[2]))

        lasso.fit <- glmnet::glmnet(x = design.matrix,
                                    y = y.response, weights = weights, lambda = lambda,
                                    alpha = alpha, penalty.factor=penalty.factor)

        lasso.coef[[nPred+1]][current+1,] = t(as.matrix(coef(lasso.fit)))
        lasso.pred[current+1, (nPred+1)] = predict(lasso.fit, newx = newx)

      } else {
        if (is.finite(alpha)) {
          lasso.fit <- glmnet::cv.glmnet(x = design.matrix,
                                         y = y.response, weights = weights, nfolds = 10, grouped = FALSE,
                                         alpha = alpha, penalty.factor = penalty.factor)
        } else {
          lasso.fit <- lm(y.response ~ ., data = data.frame(design.matrix))
        }
        if (is.finite(alpha)) {
          lasso.coef[[nPred+1]][current+1,] = t(as.matrix(coef(lasso.fit, lambda = lasso.fit$lambda.1se)))
          lasso.pred[current+1, (nPred+1)] = predict(lasso.fit, newx = newx, s = "lambda.1se")
        } else {
          lasso.coef[[nPred+1]][current+1,] = t(as.matrix(coef(lasso.fit)))
          colnames(newx)=colnames(design.matrix)
          newx <- as.data.frame(newx)
          lasso.pred[current+1, (nPred+1)] = predict(lasso.fit, newdata = newx)
        }
      }
    }
    colnames(lasso.coef[[nPred+1]]) = rownames(as.matrix(coef(lasso.fit, lambda = lasso.fit$lambda.1se)))
    lasso.coef[[nPred+1]] = lasso.coef[[nPred+1]][max(start.id,n.training+nPred+1):end.id,]
  }
  xts.pred = xts::xts(lasso.pred,time(data))
  xts.pred = xts.pred[start.id:end.id]
  print(time(xts.pred))
  result = list(pred = xts.pred, coef = lasso.coef)
}
