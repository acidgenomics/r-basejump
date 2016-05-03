install.packages("devtools")
library(devtools)

install.packages("roxygen2")
library(roxygen2)

install.packages("lintr")
library(lintr)
lintr::lint_package()

devtools::use_data_raw()
devtools::use_travis()
devtools::check()
