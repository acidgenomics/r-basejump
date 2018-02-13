context("detectOrganism")

loadRemoteData("http://basejump.seq.cloud/counts.rda")

test_that("Homo sapiens", {
    x <- "Homo sapiens"
    expect_identical(x, detectOrganism("Homo sapiens"))
    expect_identical(x, detectOrganism("hsapiens"))
    expect_identical(x, detectOrganism("GRCh38"))
    expect_identical(x, detectOrganism("grch38"))
    expect_identical(x, detectOrganism("hg38"))
    expect_identical(x, detectOrganism("ENSG00000000001"))
    expect_identical(x, detectOrganism("ENST00000000001"))
})

test_that("Mus musculus", {
    x <- "Mus musculus"
    expect_identical(x, detectOrganism("Mus musculus"))
    expect_identical(x, detectOrganism("mmusculus"))
    expect_identical(x, detectOrganism("GRCm38"))
    expect_identical(x, detectOrganism("grcm38"))
    expect_identical(x, detectOrganism("mm10"))
    expect_identical(x, detectOrganism("ENSMUSG00000000001"))
    expect_identical(x, detectOrganism("ENSMUST00000000001"))
})

test_that("Rattus norvegicus", {
    x <- "Rattus norvegicus"
    expect_identical(x, detectOrganism("Rattus norvegicus"))
    expect_identical(x, detectOrganism("rnorvegicus"))
    expect_identical(x, detectOrganism("ENSRNOG00000000001"))
    expect_identical(x, detectOrganism("ENSRNOT00000000001"))
})

test_that("Danio rerio", {
    x <- "Danio rerio"
    expect_identical(x, detectOrganism("Danio rerio"))
    expect_identical(x, detectOrganism("drerio"))
    expect_identical(x, detectOrganism("GRCz10"))
    expect_identical(x, detectOrganism("danRer10"))
    expect_identical(x, detectOrganism("ENSDARG00000000001"))
    expect_identical(x, detectOrganism("ENSDART00000000001"))
})

test_that("Drosophila melanogaster", {
    x <- "Drosophila melanogaster"
    expect_identical(x, detectOrganism("Drosophila melanogaster"))
    expect_identical(x, detectOrganism("dmelanogaster"))
    expect_identical(x, detectOrganism("BDGP6"))
    expect_identical(x, detectOrganism("dm6"))
    expect_identical(x, detectOrganism("FBgn0000001"))
    expect_identical(x, detectOrganism("FBtr0000001"))
})

test_that("Caenorhabditis elegans", {
    x <- "Caenorhabditis elegans"
    expect_identical(x, detectOrganism("Caenorhabditis elegans"))
    expect_identical(x, detectOrganism("celegans"))
    expect_identical(x, detectOrganism("WBcel235"))
    expect_identical(x, detectOrganism("ce11"))
    expect_identical(x, detectOrganism("WBGene00000001"))
})

test_that("Gallus gallus", {
    x <- "Gallus gallus"
    expect_identical(x, detectOrganism("Gallus gallus"))
    expect_identical(x, detectOrganism("ggallus"))
    expect_identical(x, detectOrganism("ENSGALG00000000001"))
    expect_identical(x, detectOrganism("ENSGALT00000000001"))
})

test_that("Ovis aries", {
    x <- "Ovis aries"
    expect_identical(x, detectOrganism("Ovis aries"))
    expect_identical(x, detectOrganism("oaries"))
    expect_identical(x, detectOrganism("ENSOARG00000000001"))
    expect_identical(x, detectOrganism("ENSOART00000000001"))
})

test_that("Multiple organisms", {
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

test_that("Detection failure", {
    expect_error(
        detectOrganism("XXX"),
        "Failed to detect organism"
    )
})

test_that("Matrix", {
    expect_identical(
        detectOrganism(counts),
        "Mus musculus"
    )
})

test_that("Tibble", {
    tibble <- as(counts, "tibble")
    expect_true("rowname" %in% colnames(tibble))
    expect_identical(
        detectOrganism(tibble),
        "Mus musculus"
    )
})
