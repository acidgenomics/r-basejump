library(devtools)
library(Matrix)
library(scater)
load_all()
single_cell_counts <- scater::sc_example_counts %>%
    as("dgCMatrix") %>%
    camel(rownames = TRUE, colnames = TRUE)
use_data(single_cell_counts, overwrite = TRUE, compress = "xz")
writeCounts(single_cell_counts, dir = "~")
