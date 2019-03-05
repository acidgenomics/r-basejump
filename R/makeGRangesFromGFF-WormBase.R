# WormBase



.makeGenesFromWormBaseGTF <- function(object) {
    assert(is(object, "GRanges"))
    object <- .makeGenesFromEnsemblGTF(object)

    # WormBase identifier fix. WormBase GTF currently imports somewhat
    # malformed, and the gene identifiers require additional sanitization to
    # return correctly. Some garbage rows containing "Gene:" or "Transcript:"
    # will remain. We need to drop these before proceeding.
    keep <- !grepl(pattern = ":", x = mcols(object)[["gene_id"]])
    object <- object[keep]

    object
}



# Note that this will contain garbage "Gene:" and "Transcript:" rows in the
# "gene_id" column, but "transcript_id" is unique. So keeping them currently.
# Might need to reevaluate this step. Check against corresponding ensembldb.
.makeTranscriptsFromWormBaseGTF <- .makeTranscriptsFromEnsemblGTF
