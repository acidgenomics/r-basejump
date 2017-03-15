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
#' @param summaryDir Summary directory
#' @param summary Summary \code{data.frame}
#'
#' @return txi \code{tximport} count data from \code{sailfish}
#' @export
#'
#' @examples
#' \dontrun{
#' bcbioSailfish(summary_dir, summary)
#' }
bcbioSailfish <- function(summaryDir, summary) {
    tx2gene <- file.path(summaryDir, "tx2gene.csv") %>%
        utils::read.csv(header = FALSE)
    finalDir <- dirname(summaryDir)

    # Parse the HPC sailfish directories
    sailfishFiles <- dir(finalDir) %>%
        .[. %in% summary$description] %>%
        file.path(finalDir,
                  .,
                  "sailfish",
                  "quant",
                  "quant.sf") %>%
        sort
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
