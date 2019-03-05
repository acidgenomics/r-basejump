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
.detectWormBaseGTFGarbage <- function(object) {
    genes <- mcols(object)[["gene_id"]]
    garbage <- grep(pattern = ":", x = genes, value = TRUE)
    if (hasLength(garbage)) {
        warning(paste(
            length(garbage), "malformed identifiers:",
            toString(sort(garbage), width = 200L)
        ))
    }
    invisible()
}



.makeGenesFromWormBaseGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- .makeGenesFromEnsemblGTF(object)
    .detectWormBaseGTFGarbage(object)
    object
}



.makeTranscriptsFromWormBaseGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- .makeTranscriptsFromEnsemblGTF(object)
    .detectWormBaseGTFGarbage(object)
    object
}
