# GENCODE
#
# Uses `gene_type` instead of `gene_biotype`.
# Note that `gene_id` and `gene_name` are nicely defined, so don't use `Name`.
#
# GTF
#>  [1] "source"                   "type"
#>  [3] "score"                    "phase"
#>  [5] "gene_id"                  "gene_type"
#>  [7] "gene_name"                "level"
#>  [9] "havana_gene"              "transcript_id"
#> [11] "transcript_type"          "transcript_name"
#> [13] "transcript_support_level" "tag"
#> [15] "havana_transcript"        "exon_number"
#> [17] "exon_id"                  "ont"
#> [19] "protein_id"               "ccdsid"
#
# GFF
#>  [1] "source"                   "type"
#>  [3] "score"                    "phase"
#>  [5] "ID"                       "gene_id"
#>  [7] "gene_type"                "gene_name"
#>  [9] "level"                    "havana_gene"
#> [11] "Parent"                   "transcript_id"
#> [13] "transcript_type"          "transcript_name"
#> [15] "transcript_support_level" "tag"
#> [17] "havana_transcript"        "exon_number"
#> [19] "exon_id"                  "ont"
#> [21] "protein_id"               "ccdsid"



.makeGenesFromGencodeGFF3 <- function(object, singlePAR = TRUE) {
    assert(
        is(object, "GRanges"),
        isFlag(singlePAR),
        isSubset(
            x = c("gene_id", "gene_name", "gene_biotype"),
            y = colnames(mcols(object))
        )
    )

    # Only keep rows that match gene type.
    object <- object[mcols(object)[["type"]] == "gene"]

    # Remove duplicate PARs on Y chromosome.
    # This is defined in the "ID" column, not "gene_id".
    if (isTRUE(singlePAR)) {
        object <- .singlePAR(object, idCol = "ID")
    }

    object <- .minimizeGFF3(object)
    object
}



.makeGenesFromGencodeGTF <- function(object, singlePAR = TRUE) {
    assert(
        is(object, "GRanges"),
        isFlag(singlePAR)
    )
    object <- .makeGenesFromEnsemblGTF(object)
    # Remove duplicate PARs on Y chromosome.
    if (isTRUE(singlePAR)) {
        object <- .singlePAR(object, idCol = "gene_id")
    }
    object
}



.makeTranscriptsFromGencodeGFF3 <- function(object, singlePAR = TRUE) {
    assert(
        is(object, "GRanges"),
        isFlag(singlePAR),
        isSubset(
            x = c(
                "gene_biotype", "gene_id",
                "transcript_biotype", "transcript_id"
            ),
            y = colnames(mcols(object))
        )
    )

    # Match "transcript" type.
    object <- object[mcols(object)[["type"]] == "transcript"]

    # Remove duplicate PARs on Y chromosome.
    # This is defined in the "ID" column, not "transcript_id".
    if (isTRUE(singlePAR)) {
        object <- .singlePAR(object, idCol = "ID")
    }

    object <- .minimizeGFF3(object)
    object
}



.makeTranscriptsFromGencodeGTF <- function(object, singlePAR = TRUE) {
    assert(
        is(object, "GRanges"),
        isFlag(singlePAR)
    )
    object <- .makeTranscriptsFromEnsemblGTF(object)

    # Remove duplicate PARs on Y chromosome.
    if (isTRUE(singlePAR)) {
        object <- .singlePAR(object, idCol = "transcript_id")
    }

    object
}



# Match Ensembl spec by removing the duplicate PAR Y chromosome annotations.
.singlePAR <- function(object, idCol) {
    assert(is(object, "GRanges"))
    idCol <- match.arg(
        arg = idCol,
        choices = c("ID", "gene_id", "transcript_id")
    )
    parY <- grepl(pattern = "_PAR_Y$", x = mcols(object)[[idCol]])
    if (hasLength(parY)) {
        message(paste(
            "Removing", sum(parY, na.remove = TRUE),
            "pseudoautosomal region (PAR) Y chromosome duplicates."
        ))
        object <- object[!parY]
    }
    object
}



.standardizeGencodeToEnsembl <- function(object) {
    assert(is(object, "GRanges"))
    mcolnames <- colnames(mcols(object))
    # Match Ensembl spec, which uses `*_biotype` instead of `*_type`.
    mcolnames <- sub(
        pattern = "^gene_type$",
        replacement = "gene_biotype",
        x = mcolnames
    )
    mcolnames <- sub(
        pattern = "^transcript_type$",
        replacement = "transcript_biotype",
        x = mcolnames
    )
    colnames(mcols(object)) <- mcolnames
    object
}
