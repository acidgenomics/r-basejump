#' Import RNA-Seq data from a bcbio project
#'
#' @author Michael Steinbaugh
#'
#' @param project bcbio project
#'
#' @return List containing RNA-Seq data
#' @export
#'
#' @examples
#' \dontrun{
#' rnaseq <- bcbioRnaseqData(project)
#' }
bcbioRnaseqData <- function(project) {
    # `.counts` = `featureCounts`
    annotated_combined.counts <-
        bcbioData(project,
                  file = "annotated_combined.counts")
    combined.counts <-
        bcbioData(project,
                  file = "combined.counts",
                  output = "matrix",
                  rownames = "id")

    # `.sf` = `sailfish`
    # Don't import the `combined.sf` file. Use the `tximport()` method on the
    # sailfish data in the sample folders instead.
    combined.gene.sf.tpm <-
        bcbioData(project,
                  file = "combined.gene.sf.tpm",
                  output = "matrix",
                  rownames = "gene_id")
    combined.isoform.sf.tpm <-
        bcbioData(project,
                  file = "combined.isoform.sf.tpm",
                  output = "matrix",
                  rownames = "id")

    list(annotated_combined.counts = annotated_combined.counts,
         combined.counts = combined.counts,
         combined.gene.sf.tpm = combined.gene.sf.tpm,
         combined.isoform.sf.tpm = combined.isoform.sf.tpm) %>%
        return
}
