library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(tspgen)
}

test_dir("tests/testthat")
