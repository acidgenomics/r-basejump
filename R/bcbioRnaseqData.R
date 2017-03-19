#' Import RNA-Seq data from a bcbio project
#' 
#' @author Michael Steinbaugh
#' @keywords bcbio import
#' 
#' @param project bcbio project
#' 
#' @return List containing RNA-Seq data
#' 
#' @examples
#' \dontrun{
#' rnaseq <- bcbioRnaseqData(project)
#' }
bcbioRnaseqData <- function(project) {
    # `.counts` = `featureCounts`
    annotated_combined.counts <-
        bcbioFile(project,
                  file = "annotated_combined.counts")
    combined.counts <-
        bcbioFile(project,
                  file = "combined.counts",
                  output = "matrix",
                  rownames = "id")
    
    # `.sf` = `sailfish`
    # Don't import the `combined.sf` file. Use the `tximport()` method on the
    # sailfish data in the sample folders instead.
    combined.gene.sf.tpm <-
        bcbioFile(project,
                  file = "combined.gene.sf.tpm",
                  output = "matrix",
                  rownames = "gene_id")
    combined.isoform.sf.tpm <-
        bcbioFile(project,
                  file = "combined.isoform.sf.tpm",
                  output = "matrix",
                  rownames = "id")
    
    list(annotated_combined.counts = annotated_combined.counts,
         combined.counts = combined.counts,
         combined.gene.sf.tpm = combined.gene.sf.tpm,
         combined.isoform.sf.tpm = combined.isoform.sf.tpm) %>%
        return
}
