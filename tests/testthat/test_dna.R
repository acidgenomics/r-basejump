context("DNA Sequence Utilities")

dna <- "ATGCATGC"

test_that("comp", {
    expect_equal(comp(dna),"TACGTACG")
    expect_error(
        comp("XXX"),
        "DNA string must only contain ACGT nucleotides")
})

test_that("revcomp", {
    expect_equal(revcomp(dna), "GCATGCAT")
    expect_error(
        revcomp("XXX"),
        "DNA string must only contain ACGT nucleotides")
})
