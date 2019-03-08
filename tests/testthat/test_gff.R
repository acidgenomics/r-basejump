context("GFF file import")

levels <- c("genes", "transcripts")

files <- c(
    ensemblGFF  = "ensembl.gff3.gz",
    ensemblGTF  = "ensembl.gtf.gz",
    flybaseGTF  = "flybase.gtf.gz",
    gencodeGFF  = "gencode.gff3.gz",
    gencodeGTF  = "gencode.gtf.gz",
    refseqGFF   = "refseq.gff.gz",
    wormbaseGTF = "wormbase.gtf.gz"
)
# Note that `file.path()` call drops names.
names <- names(files)
files <- file.path(extdata, files)
names(files) <- names
rm(names)

# nolint start
#
# `devtools::check()` is currently choking on this call.
# > files <- system.file("extdata", files, package = "basejump")
#
# ── 1. Error: (unknown) (@test_gff.R#18)  ───────────────────────────────────────
# 'names' attribute [7] must be the same length as the vector [1]
# 1: .handleSimpleError(function (e)
#    {
#        handled <<- TRUE
#        test_error <<- e
#        options(expressions = expressions_opt_new)
#        on.exit(options(expressions = expressions_opt), add = TRUE)
#        e$expectation_calls <- frame_calls(11, 2)
#        test_error <<- e
#        register_expectation(e)
#        e$handled <- TRUE
#        test_error <<- e
#    }, "'names' attribute [7] must be the same length as the vector [1]", quote(names(files)
# <- names)) at testthat/test_gff.R:18
# 2: eval(code, test_env)
#
# nolint end
skip_if_not(all(file.exists(files)))



# Ensembl ======================================================================
with_parameters_test_that(
    "Ensembl GFF", {
        file <- files[["ensemblGFF"]]
        object <- makeGRangesFromGFF(file = file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(53L, 147L)
)

with_parameters_test_that(
    "Ensembl GTF", {
        file <- files[["ensemblGTF"]]
        object <- makeGRangesFromGFF(file = file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)



# FlyBase ======================================================================
with_parameters_test_that(
    "FlyBase GTF", {
        file <- files[["flybaseGTF"]]
        expect_warning(
            makeGRangesFromGFF(file, level = level),
            "GRanges does not contain biotype in mcols\\(\\)."
        )
        suppressWarnings(
            object <- makeGRangesFromGFF(file, level = level)
        )
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(39L, 72L)
)



# GENCODE ======================================================================
with_parameters_test_that(
    "GENCODE GFF", {
        file <- files[["gencodeGFF"]]
        object <- makeGRangesFromGFF(file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)

with_parameters_test_that(
    "GENCODE GTF", {
        file <- files[["gencodeGTF"]]
        object <- makeGRangesFromGFF(file, level = level)
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(60L, 167L)
)



# WormBase =====================================================================
with_parameters_test_that(
    "WormBase GTF", {
        file <- files[["wormbaseGTF"]]
        expect_warning(
            makeGRangesFromGFF(file = file, level = level),
            "geneName"
        )
        suppressWarnings(
            object <- makeGRangesFromGFF(file = file, level = level)
        )
        expect_s4_class(object, "GRanges")
        expect_length(object, length)
    },
    level = levels,
    length = c(66L, 83L)
)



# RefSeq =======================================================================
test_that("RefSeq GFF", {
    file <- files[["refseqGFF"]]

    # Genes
    expect_warning(
        makeGRangesFromGFF(file = file, level = "genes"),
        "RefSeq support is experimental."
    )
    suppressWarnings(
        object <- makeGRangesFromGFF(file = file, level = "genes")
    )
    expect_s4_class(object, "GRanges")
    expect_length(object, 62L)

    # Transcripts
    suppressWarnings(
        object <- makeGRangesFromGFF(file = file, level = "transcripts")
    )
    expect_s4_class(object, "GRangesList")
    expect_length(object, 100L)
})
