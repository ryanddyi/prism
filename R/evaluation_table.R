#' Out-of-sample prediction evaluation
#'
#' @param claim_data  the output of load_claim_data().
#' @param prism_prediction the output of back_test().
#'
#' @export

evaluation_table<-function(claim_data, prism_prediction){
  prediction = prism_prediction$prediction
  nPred = prism_prediction$nPred
  start = which(time(claim_data$claim.all) == time(xts::first(prediction)))
  end = which(time(claim_data$claim.all) == time(xts::last(prediction)))

  diff=as.numeric(prediction)-claim_data$claim.all$icnsa[start:end+nPred]
  diff.naive=as.numeric(claim_data$claim.all$icnsa[start:end-1]) - claim_data$claim.all$icnsa[start:end+nPred]

  Years=as.numeric(format(time(diff),'%Y'))

  yearly=2007:2016
  summary=array(dim=c(2,1+length(yearly)))
  rownames(summary)=c('RMSE','MAE')
  colnames(summary)=c('whole period',yearly)

  summary[1,1]=sqrt(mean(diff^2)/mean(diff.naive^2))
  summary[2,1]=mean(abs(diff))/mean(abs(diff.naive))

  for(k in 1:length(yearly)){
    summary[1,k+1]=sqrt(mean(diff[Years==yearly[k]]^2)/mean(diff.naive[Years==yearly[k]]^2))
    summary[2,k+1]=mean(abs(diff[Years==yearly[k]]))/mean(abs(diff.naive[Years==yearly[k]]))
  }

  summary
}
