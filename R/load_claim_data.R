#' Load unemployment initial claims data
#'
#' Load weekly unemployment initial claim data (each week ends on the Saturday).
#'
#' @param GT.startDate start date of claim data
#' @param GT.endDate end date of claim data
#'
#' @return A list of following named xts objects
#' \itemize{
#' \item \code{claim.data} unemployment initial claim data from GT.startDate to GT.endDate.
#' \item \code{claim.all} load all unemployment initial claim data since 1967
#' \item \code{claim.early} unemployment initial claim data prior to GT.startDate
#' }
#'
#' @importFrom utils read.csv
#'
#' @export


load_claim_data<-function(GT.startDate = "2004-01-03", GT.endDate = "2016-12-31"){

  icnsa=read.csv(system.file("extdata", "ICNSA.csv", package = "PRISM.forecast"), header=T)

  # load all claim data
  fmt <-  "%Y-%m-%d"
  date.all = as.Date(icnsa[,1],fmt)
  claim.all = xts::xts(icnsa[,2],date.all)
  colnames(claim.all) <- "icnsa"

  # early data prior to GT data is available
  startDate = '1980-01-05'
  fmt='%Y-%m-%d'
  startIdx = which(time(claim.all)==as.Date(startDate,fmt))
  endIdx = which(time(claim.all)==as.Date(GT.startDate,fmt))-1

  claim.earlyData=claim.all[startIdx:endIdx]

  # claim data when GT data is available
  startIdx=which(time(claim.all)==as.Date(GT.startDate,fmt))
  endIdx=which(time(claim.all)==as.Date(GT.endDate,fmt))

  claim.data=claim.all[startIdx:endIdx]

  data_all <- list()
  data_all$claim.earlyData <- claim.earlyData
  data_all$claim.all <- claim.all
  data_all$claim.data <- claim.data
  data_all
}
