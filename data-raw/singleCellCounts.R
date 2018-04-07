library(devtools)
library(Matrix)
library(scater)
load_all()
singleCellCounts <- scater::sc_example_counts %>%
    as("dgCMatrix") %>%
    camel(rownames = TRUE, colnames = TRUE)
writeCounts(singleCellCounts, dir = "~")
use_data(singleCellCounts, overwrite = TRUE, compress = "xz")
