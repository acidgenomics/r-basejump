# devtools::install_github("stephenturner/annotables")

devtools::load_all()
library(magrittr)

grch37 <- annotable(annotables::grch37)
lapply(grch37, class)

grch37Tx2gene <- annotables::grch37_tx2gene %>%
    as.data.frame() %>%
    set_rownames(.[["enstxp"]])
lapply(grch37Tx2gene, class)

devtools::use_data(grch37, grch37Tx2gene, compress = "xz", overwrite = TRUE)
