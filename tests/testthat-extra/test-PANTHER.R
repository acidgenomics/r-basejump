context("extra | PANTHER")

organisms <- names(.pantherMappings)

## Here's how to obtain supported releases.
## > releases <- .pantherReleases
## > releases <- setdiff(releases, "current_release")

with_parameters_test_that(
    "PANTHER : 11.0", {
        object <- PANTHER(organism = organism, release = "11.0")
        expect_s4_class(object, "PANTHER")
        expect_identical(nrow(object), nrow)
    },
    organism = organisms,
    nrow = c(
        caenorhabditis_elegans = 15010L,
        drosophila_melanogaster = 10610L,
        homo_sapiens = 19730L,
        mus_musculus = 21112L
    )
)

with_parameters_test_that(
    "PANTHER : 12.0", {
        object <- PANTHER(organism = organism, release = "12.0")
        expect_s4_class(object, "PANTHER")
        expect_identical(nrow(object), nrow)
    },
    organism = organisms,
    nrow = c(
        caenorhabditis_elegans = 14127L,
        drosophila_melanogaster = 10030L,
        homo_sapiens = 19537L,
        mus_musculus = 20908L
    )
)

with_parameters_test_that(
    "PANTHER : 13.0", {
        object <- PANTHER(organism = organism, release = "13.0")
        expect_s4_class(object, "PANTHER")
        expect_identical(nrow(object), nrow)
    },
    organism = organisms,
    nrow = c(
        caenorhabditis_elegans = 13494L,
        drosophila_melanogaster = 10087L,
        homo_sapiens = 18577L,
        mus_musculus = 19860L
    )
)

with_parameters_test_that(
    "PANTHER : 13.1", {
        object <- PANTHER(organism = organism, release = "13.1")
        expect_s4_class(object, "PANTHER")
        expect_identical(nrow(object), nrow)
    },
    organism = organisms,
    nrow = c(
        caenorhabditis_elegans = 13806L,
        drosophila_melanogaster = 10349L,
        homo_sapiens = 19648L,
        mus_musculus = 20967L
    )
)

with_parameters_test_that(
    "PANTHER : 14.0", {
        object <- PANTHER(organism = organism, release = "14.0")
        expect_s4_class(object, "PANTHER")
        expect_identical(nrow(object), nrow)
    },
    organism = organisms,
    nrow = c(
        caenorhabditis_elegans = 14298L,
        drosophila_melanogaster = 10547L,
        homo_sapiens = 19746L,
        mus_musculus = 21113L
    )
)

with_parameters_test_that(
    "PANTHER : current_release", {
        object <- PANTHER(organism = organism, release = NULL)
        expect_s4_class(object, "PANTHER")
        message(sprintf("%s: %d", organism, nrow(object)))
    },
    organism = organisms
)
