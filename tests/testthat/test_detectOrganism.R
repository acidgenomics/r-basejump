context("detectOrganism")

test_that("Homo sapiens", {
    x <- c(human = "Homo sapiens")
    expect_identical(x, detectOrganism("Homo sapiens"))
    expect_identical(x, detectOrganism("hsapiens"))
    expect_identical(x, detectOrganism("human"))
    expect_identical(x, detectOrganism("GRCh38"))
    expect_identical(x, detectOrganism("grch38"))
    expect_identical(x, detectOrganism("hg38"))
    expect_identical(x, detectOrganism("ENSG00000000001"))
    expect_identical(x, detectOrganism("ENST00000000001"))
})

test_that("Mus musculus", {
    x <- c(mouse = "Mus musculus")
    expect_identical(x, detectOrganism("Mus musculus"))
    expect_identical(x, detectOrganism("mmusculus"))
    expect_identical(x, detectOrganism("mouse"))
    expect_identical(x, detectOrganism("GRCm38"))
    expect_identical(x, detectOrganism("grcm38"))
    expect_identical(x, detectOrganism("mm10"))
    expect_identical(x, detectOrganism("ENSMUSG00000000001"))
    expect_identical(x, detectOrganism("ENSMUST00000000001"))
})

test_that("Rattus norvegicus", {
    x <- c(rat = "Rattus norvegicus")
    expect_identical(x, detectOrganism("Rattus norvegicus"))
    expect_identical(x, detectOrganism("rnorvegicus"))
    expect_identical(x, detectOrganism("rat"))
    expect_identical(x, detectOrganism("ENSRNOG00000000001"))
    expect_identical(x, detectOrganism("ENSRNOT00000000001"))
})

test_that("Danio rerio", {
    x <- c(zebrafish = "Danio rerio")
    expect_identical(x, detectOrganism("Danio rerio"))
    expect_identical(x, detectOrganism("drerio"))
    expect_identical(x, detectOrganism("zebrafish"))
    expect_identical(x, detectOrganism("GRCz10"))
    expect_identical(x, detectOrganism("danRer10"))
    expect_identical(x, detectOrganism("ENSDARG00000000001"))
    expect_identical(x, detectOrganism("ENSDART00000000001"))
})

test_that("Drosophila melanogaster", {
    x <- c(fruitfly = "Drosophila melanogaster")
    expect_identical(x, detectOrganism("Drosophila melanogaster"))
    expect_identical(x, detectOrganism("dmelanogaster"))
    expect_identical(x, detectOrganism("fruitfly"))
    expect_identical(x, detectOrganism("BDGP6"))
    expect_identical(x, detectOrganism("dm6"))
    expect_identical(x, detectOrganism("FBgn0000001"))
    expect_identical(x, detectOrganism("FBtr0000001"))

})

test_that("Caenorhabditis elegans", {
    x <- c(roundworm = "Caenorhabditis elegans")
    expect_identical(x, detectOrganism("Caenorhabditis elegans"))
    expect_identical(x, detectOrganism("celegans"))
    expect_identical(x, detectOrganism("roundworm"))
    expect_identical(x, detectOrganism("WBcel235"))
    expect_identical(x, detectOrganism("ce11"))
    expect_identical(x, detectOrganism("WBGene00000001"))
})

test_that("Gallus gallus", {
    x <- c(chicken = "Gallus gallus")
    expect_identical(x, detectOrganism("Gallus gallus"))
    expect_identical(x, detectOrganism("ggallus"))
    expect_identical(x, detectOrganism("chicken"))
    expect_identical(x, detectOrganism("ENSGALG00000000001"))
    expect_identical(x, detectOrganism("ENSGALT00000000001"))
})

test_that("Ovis aries", {
    x <- c(sheep = "Ovis aries")
    expect_identical(x, detectOrganism("Ovis aries"))
    expect_identical(x, detectOrganism("oaries"))
    expect_identical(x, detectOrganism("sheep"))
    expect_identical(x, detectOrganism("ENSOARG00000000001"))
    expect_identical(x, detectOrganism("ENSOART00000000001"))
})

test_that("Unsupported organism", {
    x <- suppressWarnings(detectOrganism("XXX"))
    expect_identical(x, NULL)
    expect_warning(
        detectOrganism("XXX"),
        "Failed to detect supported organism"
    )
})

test_that("NULL input", {
    expect_identical(
        detectOrganism(NULL),
        NULL
    )
})
