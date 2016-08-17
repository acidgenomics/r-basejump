multimerge <- function() {
  Reduce(
    function(...) {
      merge(..., by = "ensembl_gene_id", all = TRUE)
    },
    list(bm1, bm2, bm3)
  )
}
