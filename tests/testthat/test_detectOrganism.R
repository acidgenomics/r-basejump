context("detectOrganism")

test_that("human", {
    human <- detectOrganism("Homo sapiens")
    expect_identical(human, c(human = "Homo sapiens"))
    expect_equal(human, detectOrganism("hsapiens"))
    expect_equal(human, detectOrganism("human"))
    expect_equal(human, detectOrganism("GRCh38"))
    expect_equal(human, detectOrganism("grch38"))
    expect_equal(human, detectOrganism("hg38"))
    expect_equal(human, detectOrganism("ENSG00000000001"))
    expect_equal(human, detectOrganism("ENST00000000001"))
})

test_that("mouse", {
    mouse <- detectOrganism("mouse")
    expect_identical(mouse, c(mouse = "Mus musculus"))
    expect_equal(mouse, detectOrganism("mmusculus"))
    expect_equal(mouse, detectOrganism("mouse"))
    expect_equal(mouse, detectOrganism("GRCm38"))
    expect_equal(mouse, detectOrganism("grcm38"))
    expect_equal(mouse, detectOrganism("mm10"))
    expect_equal(mouse, detectOrganism("ENSMUSG00000000001"))
    expect_equal(mouse, detectOrganism("ENSMUST00000000001"))
})

test_that("zebrafish", {
    zebrafish <- detectOrganism("zebrafish")
    expect_identical(zebrafish, c(zebrafish = "Danio rerio"))
    expect_equal(zebrafish, detectOrganism("drerio"))
    expect_equal(zebrafish, detectOrganism("zebrafish"))
    expect_equal(zebrafish, detectOrganism("GRCz10"))
    expect_equal(zebrafish, detectOrganism("danRer10"))
    expect_equal(zebrafish, detectOrganism("ENSDARG00000000001"))
    expect_equal(zebrafish, detectOrganism("ENSDART00000000001"))
})

test_that("fruitfly", {
    fruitfly <- detectOrganism("fruitfly")
    expect_identical(fruitfly, c(fruitfly = "Drosophila melanogaster"))
    expect_equal(fruitfly, detectOrganism("dmelanogaster"))
    expect_equal(fruitfly, detectOrganism("fruitfly"))
    expect_equal(fruitfly, detectOrganism("BDGP6"))
    expect_equal(fruitfly, detectOrganism("dm6"))
    expect_equal(fruitfly, detectOrganism("FBgn0000001"))
    expect_equal(fruitfly, detectOrganism("FBtr0000001"))

})

test_that("roundworm", {
    roundworm <- detectOrganism("roundworm")
    expect_identical(roundworm, c(roundworm = "Caenorhabditis elegans"))
    expect_equal(roundworm, detectOrganism("celegans"))
    expect_equal(roundworm, detectOrganism("roundworm"))
    expect_equal(roundworm, detectOrganism("WBcel235"))
    expect_equal(roundworm, detectOrganism("ce11"))
    expect_equal(roundworm, detectOrganism("WBGene00000001"))
})

test_that("chicken", {
    chicken <- detectOrganism("chicken")
    expect_identical(chicken, c(chicken = "Gallus gallus"))
    expect_equal(chicken, detectOrganism("ggallus"))
    expect_equal(chicken, detectOrganism("chicken"))
    expect_equal(chicken, detectOrganism("ENSGALG00000000001"))
    expect_equal(chicken, detectOrganism("ENSGALT00000000001"))
})

test_that("rat", {
    rat <- detectOrganism("rat")
    expect_identical(rat, c(rat = "Rattus norvegicus"))
    expect_equal(rat, detectOrganism("rnorvegicus"))
    expect_equal(rat, detectOrganism("rat"))
    expect_equal(rat, detectOrganism("ENSRNOG00000000001"))
    expect_equal(rat, detectOrganism("ENSRNOT00000000001"))
})

test_that("unsupported", {
    x <- suppressWarnings(detectOrganism("XXX"))
    expect_equal(x, NULL)
    expect_warning(
        detectOrganism("XXX"),
        "Failed to detect supported organism"
    )
})
