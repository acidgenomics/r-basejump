context("Infrastructure Utilities")

test_that("detectHPC", {
    expect_equal(detectHPC(), FALSE)
})



test_that("detectOrganism",  {
    # Organisms supported in annotables
    human <- detectOrganism("human")
    expect_equal(human, c(human = "Homo sapiens"))
    expect_equal(human, detectOrganism("grch38"))
    expect_equal(human, detectOrganism("GRCh38"))
    expect_equal(human, detectOrganism("hg38"))
    expect_equal(human, detectOrganism("ENSG00000000001"))
    expect_equal(human, detectOrganism("ENST00000000001"))
    expect_equal(human, detectOrganism("human"))
    expect_equal(human, detectOrganism("hsapiens"))
    expect_equal(human, detectOrganism("Homo sapiens"))

    mouse <- detectOrganism("mouse")
    expect_equal(mouse, c(mouse = "Mus musculus"))
    expect_equal(mouse, detectOrganism("grcm38"))
    expect_equal(mouse, detectOrganism("GRCm38"))
    expect_equal(mouse, detectOrganism("mm10"))
    expect_equal(mouse, detectOrganism("ENSMUSG00000000001"))
    expect_equal(mouse, detectOrganism("ENSMUST00000000001"))
    expect_equal(mouse, detectOrganism("mouse"))
    expect_equal(mouse, detectOrganism("mmusculus"))
    expect_equal(mouse, detectOrganism("Mus musculus"))

    roundworm <- detectOrganism("roundworm")
    expect_equal(roundworm, c(roundworm = "Caenorhabditis elegans"))
    expect_equal(roundworm, detectOrganism("WBcel235"))
    expect_equal(roundworm, detectOrganism("ce11"))
    expect_equal(roundworm, detectOrganism("WBGene00000001"))
    expect_equal(roundworm, detectOrganism("roundworm"))
    expect_equal(roundworm, detectOrganism("celegans"))
    expect_equal(roundworm, detectOrganism("Caenorhabditis elegans"))

    fruitfly <- detectOrganism("fruitfly")
    expect_equal(fruitfly, c(fruitfly = "Drosophila melanogaster"))
    expect_equal(fruitfly, detectOrganism("BDGP6"))
    expect_equal(fruitfly, detectOrganism("dm6"))
    expect_equal(fruitfly, detectOrganism("FBgn0000001"))
    expect_equal(fruitfly, detectOrganism("FBtr0000001"))
    expect_equal(fruitfly, detectOrganism("fruitfly"))
    expect_equal(fruitfly, detectOrganism("dmelanogaster"))
    expect_equal(fruitfly, detectOrganism("Drosophila melanogaster"))

    chicken <- detectOrganism("chicken")
    expect_equal(chicken, c(chicken = "Gallus gallus"))
    expect_equal(chicken, detectOrganism("ENSGALG00000000001"))
    expect_equal(chicken, detectOrganism("ENSGALT00000000001"))
    expect_equal(chicken, detectOrganism("chicken"))
    expect_equal(chicken, detectOrganism("ggallus"))
    expect_equal(chicken, detectOrganism("Gallus gallus"))

    rat <- detectOrganism("rat")
    expect_equal(rat, c(rat = "Rattus norvegicus"))
    expect_equal(rat, detectOrganism("ENSRNOG00000000001"))
    expect_equal(rat, detectOrganism("ENSRNOT00000000001"))
    expect_equal(rat, detectOrganism("rat"))
    expect_equal(rat, detectOrganism("rnorvegicus"))
    expect_equal(rat, detectOrganism("Rattus norvegicus"))
})



test_that("dots", {
    expect_identical(
        dots(mtcars, starwars),
        list(sym("mtcars"),
             sym("starwars")))
    expect_identical(
        dots(mtcars, starwars, character = TRUE),
        c("mtcars", "starwars"))
    expect_error(
        dots(test, test),
        "Duplicate dots: test")
    expect_error(
        dots(test, test),
        "Duplicate dots: test")
    expect_error(
        dots(test, test, character = TRUE),
        "Duplicate dots: test")
})
