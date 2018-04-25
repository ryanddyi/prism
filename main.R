library('devtools')
library('roxygen2')

setwd("/Users/ydd/Documents/macroEcon/package")

setwd('./PRISM')
document()

setwd('..')
install('PRISM')

# example of out-of-sample evaluation

claim_data = PRISM::load_claim_data()

prism_prediction = PRISM::back_test()

PRISM::evaluation_table(claim_data, prism_prediction)

# example for single week prism.

folders_5y = list.files(system.file("extdata/search_data_5year", package = "PRISM"))
prism.fit = list()

folder = folders_5y[1]

prism_data = load_5y_search_data(folder)

data = prism_data$claim.data[1:200]
data[200] = NA
data.early = prism_data$claim.earlyData
GTdata = prism_data$allSearch[1:200]

result = prism(data, data.early, GTdata)
result$pred

# example for plot



