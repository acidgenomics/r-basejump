context("Gene Functions")

# detectOrganism ===============================================================
test_that("detectOrganism : Homo sapiens", {
    x <- "Homo sapiens"
    expect_identical(x, detectOrganism("Homo sapiens"))
    expect_identical(x, detectOrganism("hsapiens"))
    expect_identical(x, detectOrganism("GRCh38"))
    expect_identical(x, detectOrganism("grch38"))
    expect_identical(x, detectOrganism("hg38"))
    expect_identical(x, detectOrganism("ENSG00000000001"))
    expect_identical(x, detectOrganism("ENST00000000001"))
})

test_that("detectOrganism : Mus musculus", {
    x <- "Mus musculus"
    expect_identical(x, detectOrganism("Mus musculus"))
    expect_identical(x, detectOrganism("mmusculus"))
    expect_identical(x, detectOrganism("GRCm38"))
    expect_identical(x, detectOrganism("grcm38"))
    expect_identical(x, detectOrganism("mm10"))
    expect_identical(x, detectOrganism("ENSMUSG00000000001"))
    expect_identical(x, detectOrganism("ENSMUST00000000001"))
})

test_that("detectOrganism : Rattus norvegicus", {
    x <- "Rattus norvegicus"
    expect_identical(x, detectOrganism("Rattus norvegicus"))
    expect_identical(x, detectOrganism("rnorvegicus"))
    expect_identical(x, detectOrganism("ENSRNOG00000000001"))
    expect_identical(x, detectOrganism("ENSRNOT00000000001"))
})

test_that("detectOrganism : Danio rerio", {
    x <- "Danio rerio"
    expect_identical(x, detectOrganism("Danio rerio"))
    expect_identical(x, detectOrganism("drerio"))
    expect_identical(x, detectOrganism("GRCz10"))
    expect_identical(x, detectOrganism("danRer10"))
    expect_identical(x, detectOrganism("ENSDARG00000000001"))
    expect_identical(x, detectOrganism("ENSDART00000000001"))
})

test_that("detectOrganism : Drosophila melanogaster", {
    x <- "Drosophila melanogaster"
    expect_identical(x, detectOrganism("Drosophila melanogaster"))
    expect_identical(x, detectOrganism("dmelanogaster"))
    expect_identical(x, detectOrganism("BDGP6"))
    expect_identical(x, detectOrganism("dm6"))
    expect_identical(x, detectOrganism("FBgn0000001"))
    expect_identical(x, detectOrganism("FBtr0000001"))
})

test_that("detectOrganism : Caenorhabditis elegans", {
    x <- "Caenorhabditis elegans"
    expect_identical(x, detectOrganism("Caenorhabditis elegans"))
    expect_identical(x, detectOrganism("celegans"))
    expect_identical(x, detectOrganism("WBcel235"))
    expect_identical(x, detectOrganism("ce11"))
    expect_identical(x, detectOrganism("WBGene00000001"))
})

test_that("detectOrganism : Gallus gallus", {
    x <- "Gallus gallus"
    expect_identical(x, detectOrganism("Gallus gallus"))
    expect_identical(x, detectOrganism("ggallus"))
    expect_identical(x, detectOrganism("ENSGALG00000000001"))
    expect_identical(x, detectOrganism("ENSGALT00000000001"))
})

test_that("detectOrganism : Ovis aries", {
    x <- "Ovis aries"
    expect_identical(x, detectOrganism("Ovis aries"))
    expect_identical(x, detectOrganism("oaries"))
    expect_identical(x, detectOrganism("ENSOARG00000000001"))
    expect_identical(x, detectOrganism("ENSOART00000000001"))
})

test_that("detectOrganism : Multiple organisms", {
    multi <- c(
        "ENSG00000000001",
        "ENSG00000000002",
        "ENSMUSG00000000001",
        "ENSMUSG00000000002"
    )
    expect_identical(
        suppressWarnings(detectOrganism(multi, unique = FALSE)),
        c(
            "ENSG00000000001" = "Homo sapiens",
            "ENSG00000000002" = "Homo sapiens",
            "ENSMUSG00000000001" = "Mus musculus",
            "ENSMUSG00000000002" = "Mus musculus"
        )
    )
    expect_identical(
        suppressWarnings(detectOrganism(multi, unique = TRUE)),
        c("Homo sapiens", "Mus musculus")
    )
    expect_warning(
        detectOrganism(multi),
        "Multiple organisms detected"
    )
})

test_that("detectOrganism : Detection failure", {
    expect_error(
        detectOrganism("XXX"),
        "Failed to detect organism"
    )
})

test_that("detectOrganism : matrix", {
    expect_identical(
        detectOrganism(counts),
        "Mus musculus"
    )
})

test_that("detectOrganism : tibble", {
    tbl <- as(counts, "tibble")
    expect_true("rowname" %in% colnames(tbl))
    expect_identical(
        detectOrganism(tbl),
        "Mus musculus"
    )
})



# stripTranscriptVersions ======================================================
test_that("stripTranscriptVersions", {
    expect_identical(
        stripTranscriptVersions("ENSMUST00000119854.7"),
        "ENSMUST00000119854"
    )
    # Require detectinon of Ensembl transcript (ENS*T)
    expect_error(
        stripTranscriptVersions("EGFP.1"),
        "is_matching_regex"
    )
    # Theoretical spike-in containing a transcript version
    expect_identical(
        stripTranscriptVersions(c("ENSMUST00000119854.7", "EGFP.1")),
        c("ENSMUST00000119854", "EGFP")
    )
})



# tx2geneFromGFF ===============================================================
test_that("tx2geneFromGFF : Mouse", {
    mouse <- tx2geneFromGFF("mmusculus.gtf")
    expect_identical(dim(mouse), c(20L, 2L))
    expect_identical(
        head(mouse, 2L),
        data.frame(
            "enstxp" = c(
                "ENSMUST00000070533",
                "ENSMUST00000082908"),
            "ensgene" = c(
                "ENSMUSG00000051951",
                "ENSMUSG00000064842"),
            row.names = c(
                "ENSMUST00000070533",
                "ENSMUST00000082908"),
            stringsAsFactors = FALSE)
    )
    expect_message(
        tx2geneFromGFF("mmusculus.gtf"),
        "tx2gene mappings: 20 transcripts, 17 genes"
    )
})

test_that("tx2geneFromGFF : Fruitfly", {
    fruitfly <- tx2geneFromGFF("dmelanogaster.gtf")
    expect_identical(dim(fruitfly), c(7L, 2L))
    expect_identical(
        head(fruitfly, 2L),
        data.frame(
            "enstxp" = c(
                "FBtr0070000",
                "FBtr0070001"),
            "ensgene" = c(
                "FBgn0031081",
                "FBgn0052826"),
            row.names = c(
                "FBtr0070000",
                "FBtr0070001"),
            stringsAsFactors = FALSE)
    )
})

test_that("tx2geneFromGFF : data.frame", {
    gff <- readGFF("mmusculus.gtf")
    expect_identical(
        tx2geneFromGFF("mmusculus.gtf"),
        tx2geneFromGFF(gff)
    )
})

test_that("tx2geneFromGFF : Invalid number of columns", {
    expect_error(
        tx2geneFromGFF(mtcars),
        "are_identical : ncol\\(object\\) and 9L are not identical."
    )
})
