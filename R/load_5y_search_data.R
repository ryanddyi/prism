#' Load Goolge Trends data and initial claims data
#'
#' Load weekly unemployment initial claim data and related Google Trend data over 5-year span (each week ends on the Saturday). The list of Google search terms is the same as in paper.
#'
#' @param folder foldernames for a certain periods of Google Trends data. The foldernames are "0408", "0610", "0812", "1014", "1216", "1418", "1620". For example, the folder "0408" is for 2004-2008.
#'
#' @return A list of following named xts objects
#' \itemize{
#' \item \code{claim.data} unemployment initial claim data of the same span as Google Trend data.
#' \item \code{claim.all} load all unemployment initial claim data since 1967
#' \item \code{claim.early} unemployment initial claim data from 1980-01-06 to the start of \code{claim.data}.
#' \item \code{allSearch} Google Trends data of a span over five years. It is in the scale of 0 -- 100.
#' }
#' @importFrom zoo index
#'
#' @export

load_5y_search_data<-function(folder='0408'){

  # check if the folder name is among the available
  folders_5y = list.files(system.file("extdata/search_data_5year", package = "PRISM.forecast"))
  if (!folder %in% folders_5y) {
    print('Not available. Consider one of the following:')
    print(folders_5y)
    return(NULL)
  }

  # weekly GT data is available to a max span of 5 years.
  searchTerms = list.files(system.file("extdata/search_data_5year", folder, package = "PRISM.forecast"))

  for (i in 1:length(searchTerms)){
    search = utils::read.csv(system.file("extdata/search_data_5year", folder, searchTerms[i], package = "PRISM.forecast"), header = T)

    fmt = "%Y-%m-%d"
    date.all = as.Date(search$date,fmt)+6
    searchWeekly = xts::xts(search[,2],date.all)

    if(i >= 2){
      allSearch = cbind(allSearch,searchWeekly)
    } else{
      allSearch = searchWeekly
    }
    colnames(allSearch)[i] = colnames(search)[2]
  }

  GT.startDate = as.character(xts::first(date.all))
  GT.endDate = as.character(xts::last(date.all))
  data_5y<-load_claim_data(GT.startDate, GT.endDate)
  data_5y$allSearch = allSearch
  data_5y
}

