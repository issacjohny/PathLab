library(testthat)
library(pathlab)
results <- test_check("pathlab")
summary <- as.data.frame(results)
summary$result <- NULL
jsonlite::write_json(summary, "pathlab-test-summary.json", auto_unbox=TRUE, pretty=TRUE)
