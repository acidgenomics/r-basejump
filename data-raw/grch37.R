library(devtools)
install_github("stephenturner/annotables")
library(annotables)

load_all()

grch37 <- annotable(annotables::grch37)
lapply(grch37, class)
use_data(grch37, compress = "xz", overwrite = TRUE)
