suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(ni.datasets))

write.table(test_check("ni.datasets"), "test_results.csv")
