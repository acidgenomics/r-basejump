# FIXME Can we get RefSeq to return genes/transcripts as `GRanges` instead of
# `GRangesList`?



# RefSeq
#
# GFF
#>  [1] "source"                    "type"
#>  [3] "score"                     "phase"
#>  [5] "ID"                        "Dbxref"
#>  [7] "Name"                      "chromosome"
#>  [9] "gbkey"                     "genome"
#> [11] "mol_type"                  "description"
#> [13] "gene"                      "gene_biotype"
#> [15] "pseudo"                    "Parent"
#> [17] "product"                   "transcript_id"
#> [19] "gene_synonym"              "model_evidence"
#> [21] "protein_id"                "Note"
#> [23] "exception"                 "inference"
#> [25] "standard_name"             "experiment"
#> [27] "function"                  "regulatory_class"
#> [29] "feat_class"                "recombination_class"
#> [31] "rpt_type"                  "rpt_unit_seq"
#> [33] "anticodon"                 "partial"
#> [35] "start_range"               "end_range"
#> [37] "transl_except"             "mobile_element_type"
#> [39] "rpt_family"                "satellite"
#> [41] "bound_moiety"              "Target"
#> [43] "assembly_bases_aln"        "assembly_bases_seq"
#> [45] "bit_score"                 "blast_aligner"
#> [47] "blast_score"               "common_component"
#> [49] "e_value"                   "filter_score"
#> [51] "for_remapping"             "gap_count"
#> [53] "hsp_percent_coverage"      "matchable_bases"
#> [55] "matched_bases"             "num_ident"
#> [57] "num_mismatch"              "pct_coverage"
#> [59] "pct_coverage_hiqual"       "pct_identity_gap"
#> [61] "pct_identity_gapopen_only" "pct_identity_ungap"
#> [63] "rank"                      "weighted_identity"
#> [65] "lxr_locAcc_currStat_120"   "not_for_annotation"
#> [67] "consensus_splices"         "exon_identity"
#> [69] "identity"                  "idty"
#> [71] "matches"                   "product_coverage"
#> [73] "splices"                   "Gap"
#> [75] "merge_aligner"             "map"
#> [77] "part"                      "lxr_locAcc_currStat_35"
#> [79] "direction"                 "rpt_unit_range"
#> [81] "exon_number"               "number"
#> [83] "allele"                    "align_id"
#> [85] "batch_id"                  "crc32"
#> [87] "curated_alignment"         "promoted_rank"
#> [89] "qtaxid"                    "Is_circular"
#> [91] "country"                   "isolation-source"
#> [93] "note"                      "tissue-type"
#> [95] "codons"                    "transl_table"
#
# Types that map to `gene_id`:
#> [1] "enhancer"             "gene"
#> [2] "promoter"             "pseudogene"
#> [5] "recombination_region" "sequence_feature"
#
# Types that map to `transcript_id`:
#>  [1] "antisense_RNA"      "exon"
#>  [3] "guide_RNA"          "lnc_RNA"
#>  [5] "mRNA"               "primary_transcript"
#>  [7] "RNase_MRP_RNA"      "RNase_P_RNA"
#>  [9] "rRNA"               "scRNA"
#> [11] "snoRNA"             "snRNA"
#> [13] "telomerase_RNA"     "transcript"
#> [15] "vault_RNA"          "Y_RNA"



.makeTranscriptsFromRefSeqGFF3 <- function(object) {
    assert(is(object, "GRanges"))

    # Assign `transcript_name` from `Name` column.
    assert(
        isSubset("Name", colnames(mcols(object))),
        areDisjointSets("transcript_name", colnames(mcols(object)))
    )
    mcols(object)[["transcript_name"]] <- mcols(object)[["Name"]]

    object <- .minimizeGFF3(object)
    object
}



.makeGenesFromRefSeqGFF3 <- function(object) {
    assert(is(object, "GRanges"))

    # Drop rows that contain a `Parent` element.
    keep <- vapply(
        X = mcols(object)[["Parent"]],
        FUN = function(x) {
            identical(x, character(0))
        },
        FUN.VALUE = logical(1L)
    )
    object <- object[keep]

    # Keep rows that match gene/pseudogene type.
    keep <- grepl(
        pattern = paste(c("^gene$", "^pseudogene$"), collapse = "|"),
        x = mcols(object)[["type"]],
        ignore.case = TRUE
    )
    object <- object[keep]

    # Assign `gene_name` column using `Name` column.
    assert(
        isSubset("Name", colnames(mcols(object))),
        areDisjointSets("gene_name", colnames(mcols(object)))
    )
    mcols(object)[["gene_name"]] <- mcols(object)[["Name"]]

    object <- .minimizeGFF3(object)
    object
}



.standardizeRefSeqToEnsembl <- function(object) {
    assert(is(object, "GRanges"))
    # Rename `gene` column to `gene_id`, matching Ensembl spec.
    colnames(mcols(object)) <- sub(
        pattern = "^gene$",
        replacement = "gene_id",
        x = colnames(mcols(object))
    )
    object
}
