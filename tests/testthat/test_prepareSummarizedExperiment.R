context("prepareSummarizedExperiment")

# Create the matrix with invalid names. We'll sanitize these
# into snake_case later.
mat <- matrix(
    seq(1L:16L),
    nrow = 4L,
    ncol = 4L,
    dimnames = list(
        c("gene_1", "gene_2", "gene_3", "gene_4"),
        c("sample_1", "sample_2", "sample_3", "sample_4")))
# Set NA in rowdata here to test retired Ensembl identifier handling
rowdata <- data.frame(
    ensgene = c("Aaa", "Bbb", "Ccc", NA),
    biotype = c("coding", "coding", "coding", "pseudogene"),
    row.names = rownames(mat))
coldata <- data.frame(
    genotype = c("wt", "wt", "ko", "ko"),
    age = c(3L, 6L, 3L, 6L),
    row.names = colnames(mat))
se <- prepareSummarizedExperiment(
    assays = list(mat),
    rowData = rowdata,
    colData = coldata)

test_that("valid summarizedexperiment", {
    expect_equal(
        dim(se),
        c(4L, 4L))
    expect_equal(
        names(metadata(se)),
        c("date",
          "wd",
          "utilsSessionInfo",
          "devtoolsSessionInfo",
          "missingGenes"))
    expect_equal(
        metadata(se)[["missingGenes"]],
        "gene_4")
})

test_that("ensure assays requires a list", {
    expect_error(
        prepareSummarizedExperiment(
            assays = mat,
            rowData = rowdata,
            colData = coldata),
        paste("unable to find an inherited method for function",
              "'prepareSummarizedExperiment' for signature '\"matrix\"'"))
})

# This checks to see if there are any dashes in the names
test_that("enforce strict names", {
    expect_error(
        prepareSummarizedExperiment(
            assays = list(
                mat %>%
                    magrittr::set_rownames(
                        x = .,
                        value = gsub("_", "-", rownames(mat)))
            ),
            rowData = rowdata,
            colData = coldata),
        "Rownames are not valid.")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(
                mat %>%
                    magrittr::set_colnames(
                        x = .,
                        value = gsub("_", "-", colnames(mat)))
            ),
            rowData = rowdata,
            colData = coldata),
        "Colnames are not valid.")
})

test_that("duplicate names", {
    expect_error(
        prepareSummarizedExperiment(
            assays = list(
                mat %>%
                    magrittr::set_rownames(
                        x = .,
                        value = c("gene_1",
                          "gene_1",
                          "gene_2",
                          "gene_2"))
            ),
            rowData = rowdata,
            colData = coldata),
        "Non-unique rownames")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(
                mat %>%
                    magrittr::set_colnames(
                        x = .,
                        value = c("sample_1",
                          "sample_1",
                          "sample_2",
                          "sample_2"))
            ),
            rowData = rowdata,
            colData = coldata),
        "Non-unique colnames")
})

test_that("'dim' pass-in failure", {
    # Bad pass-in of objects not supporting `dim()`
    expect_error(
        prepareSummarizedExperiment(
            assays = list(c(xxx = "yyy")),
            rowData = rowdata,
            colData = coldata),
        "Assay object must support 'dim\\(\\)'")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(mat),
            rowData = rowdata,
            colData = c(xxx = "yyy")),
        "colData must support 'dim\\(\\)'")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(mat),
            rowData = c(xxx = "yyy"),
            colData = coldata),
        "rowData must support 'dim\\(\\)'")
})

test_that("invalid metadata", {
    expect_error(
        prepareSummarizedExperiment(
            assays = list(mat),
            rowData = rowdata,
            colData = coldata,
            metadata = Sys.Date()),
        "Metadata must be 'list' class object")
})

test_that("dimension mismatch", {
    expect_error(
        prepareSummarizedExperiment(
            assays = list(
                cbind(mat, "sample_5" = seq(17L, 20L))
            ),
            rowData = rowdata,
            colData = coldata),
        "colData mismatch with assay slot: sample_5")
    expect_warning(
        prepareSummarizedExperiment(
            assays = list(
                rbind(mat, "gene_5" = seq(17L, 20L))
            ),
            rowData = rowdata,
            colData = coldata),
        "rowData mismatch with assay slot: gene_5")
})

test_that("missing rownames", {
    # Missing rownames
    expect_error(
        prepareSummarizedExperiment(
            assays = list(
                mat %>%
                    magrittr::set_rownames(
                        x = .,
                        value = NULL)
            ),
            rowData = rowdata,
            colData = coldata),
        "Assay missing rownames")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(
                mat %>%
                    magrittr::set_colnames(
                        x = .,
                        value = NULL)
            ),
            rowData = rowdata,
            colData = coldata),
        "Assay missing colnames")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(mat),
            rowData = rowdata %>%
                magrittr::set_rownames(
                    x = .,
                    value = NULL),
            colData = coldata),
        "rowData missing rownames")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(mat),
            rowData = rowdata,
            colData = coldata %>%
                magrittr::set_rownames(
                    x = .,
                    value = NULL)),
        "colData missing rownames")
})

test_that("deprecated", {
    expect_warning(
        packageSE(
            assays = list(mat),
            colData = coldata,
            rowData = rowdata),
        "Use 'prepareSummarizedExperiment' instead.")
    expect_warning(
        prepareSE(
            assays = list(mat),
            colData = coldata,
            rowData = rowdata),
        "Use 'prepareSummarizedExperiment' instead.")

})
