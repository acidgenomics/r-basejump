# Modified from bcbio-rnaseq qc-summary tempalte
# https://github.com/roryk/bcbio.rnaseq/blob/master/resources/bcbio/qc-summary.template

#' Sailfish data
#'
#' Import sailfish data from \code{bcbio-rnaseq} run
#'
#' @author Michael Steinbaugh
#' @keywords bcbio rnaseq
#'
#' @import readr
#' @import tximport
#' @importFrom utils read.csv
#'
#' @param project bcbio project
#' @param summary Summary \code{data.frame}
#'
#' @return txi \code{tximport} count data from \code{sailfish}
#' @export
#'
#' @examples
#' \dontrun{
#' bcbioSailfish(project, summary)
#' }
bcbioSailfish <- function(project, summary) {
    if (!is.list(project)) {
        stop("bcbio project list is required.")
    }
    if (!is.data.frame(summary)) {
        stop("bcbio-rnaseq summary data.frame is required.")
    }
    if (!file.exists(file.path(project$summaryDir, "tx2gene.csv"))) {
        stop("tx2gene.csv file not found.")
    }

    tx2gene <- file.path(project$summaryDir, "tx2gene.csv") %>%
        utils::read.csv(header = FALSE)

    # Parse the HPC sailfish directories
    sailfishFiles <- dir(project$finalDir) %>%
        .[. %in% summary$description] %>%
        file.path(project$finalDir,
                  .,
                  "sailfish",
                  "quant",
                  "quant.sf") %>%
        sort
    if (!length(sailfishFiles)) {
        stop("No sailfish files were found.")
    }

    names(sailfishFiles) <- sort(summary$description)
    print(sailfishFiles)

    # Import the counts
    txi <- tximport::tximport(sailfishFiles,
                              type = "sailfish",
                              tx2gene = tx2gene,
                              reader = readr::read_tsv,
                              countsFromAbundance = "lengthScaledTPM")

    # Save binary data
    save(txi, file = "data/txi.rda")

    return(txi)
}
