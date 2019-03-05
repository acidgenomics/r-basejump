# WormBase
#
# GTF
#>  [1] "source"             "type"               "score"
#>  [4] "phase"              "gene_id"            "gene_source"
#>  [7] "gene_biotype"       "transcript_id"      "transcript_source"
#> [10] "transcript_biotype" "exon_number"        "exon_id"
#> [13] "protein_id"



# WormBase identifier fix may be needed. WormBase GTF currently imports somewhat
# malformed, and the gene identifiers require additional sanitization to return
# correctly. Some garbage rows containing "Gene:" or "Transcript:" will remain.
# Currently, we're warning the user when this occurs, and not removing.
.removeWormBaseGTFGarbage <- function(object, idCol) {
    idCol <- match.arg(idCol, choices = c("gene_id", "transcript_id"))
    ids <- mcols(object)[[idCol]]
    drop <- grepl(pattern = ":", x = ids)
    if (any(drop)) {
        garbage <- ids[which(drop)]
        warning(paste0(
            "Removed ", length(garbage), " malformed ", idCol, ": ",
            toString(sort(garbage), width = 300L)
        ))
        keep <- !drop
        object <- object[keep]
    }
    object
}



.makeGenesFromWormBaseGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- .makeGenesFromEnsemblGTF(object)
    object <- .removeWormBaseGTFGarbage(object, idCol = "gene_id")
    object
}



.makeTranscriptsFromWormBaseGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- .makeTranscriptsFromEnsemblGTF(object)
    object <- .removeWormBaseGTFGarbage(object, idCol = "transcript_id")
    object
}
