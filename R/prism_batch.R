#' PRISM stage 2 by batch
#'
#' PRISM penalized linear regression function for a range of time (only used internally for back testing)
#'
#' @param data time series of interest as xts, last element can be NA. (e.g., unemployment initial claim data in the same period as \code{GTdata}).
#' @param GTdata contemporaneous exogenous data as xts. (e.g., Google Trend data)
#' @param var generated regressors from stage 1.
#' @param n.training length of regression training period (by default = 156)
#' @param alpha penalty between lasso and ridge. alpha=1 represents lasso, alpha=0 represents ridge, alpha=NA represents no penalty.
#' @param start.date the starting date for forecast. If NULL, the forecast start at the earliest possible date.
#' @param n.weeks the number of weeks in the batch. If NULL, the forecast end at the latest possible date.
#' @param nPred.vec the number of periods ahead for forecast. nPred.vec could be a vector of intergers. e.g. nPred.vec=0:3 gives results from nowcast to 3-week ahead forecast.
#' @param UseGoogle boolean variable indicating whether to use Google Trend data.
#' @param discount exponential weighting: (1-discount)^lag (by default = 0.01)
#' @param sepL1 if TRUE, use separate L1 regularization parameters for time series components and exogenous variables (Goolgle Trend data)
#'
#' @return A list of following named objects
#' \itemize{
#' \item \code{coef} coefficients for Intercept, z.lags, seasonal.lags and exogenous variables.
#' \item \code{pred} prediction results for \code{n.weeks} from \code{start.date}.
#' }
#'
#' @importFrom zoo index
#'
#' @export

prism_batch<-function(data, GTdata, var, n.training=156, UseGoogle=T, alpha=1, nPred.vec=0:3, start.date = NULL, n.weeks = NULL, discount = 0.01, sepL1 = F){

  # n.lags determined by var
  n.lag = 1:dim(var$y.lags)[2]

  if(is.null(start.date)){
    start.id = n.training+min(nPred.vec)+1
  } else {
    start.id = max(which(time(data)<=start.date))
    if((n.training+min(nPred.vec))>=start.id){
      stop('start.date too early')
    }
  }

  if(is.null(n.weeks)){
    end.id = length(data)
  } else {
    end.id = start.id + n.weeks - 1
    if(length(data)<end.id){
      stop('end of testing too late. Consider decrease n.weeks')
    }
  }

  lasso.pred<-array(dim=c(length(data),max(nPred.vec+1)))

  colnames(lasso.pred)=paste0('forecast_',0:max(nPred.vec))

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

      penalty.factor = c(rep(1,max(n.lag)),rep(1,dim(GTdata)[2]))
      design.matrix.all = cbind(var$y.lags[,n.lag],data.matrix(GTdata))
    } else {
      penalty.factor = rep(1,max(n.lag))
      design.matrix.all=var$y.lags[,n.lag]
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
        print(time(data[current+1]))
      }
    }
    colnames(lasso.coef[[nPred+1]]) = rownames(as.matrix(coef(lasso.fit, lambda = lasso.fit$lambda.1se)))
    lasso.coef[[nPred+1]] = lasso.coef[[nPred+1]][max(start.id,n.training+nPred+1):end.id,]
  }
  xts.pred = xts::xts(lasso.pred,time(data))
  xts.pred = xts.pred[start.id:end.id]
  result = list(pred = xts.pred, coef = lasso.coef)
}


