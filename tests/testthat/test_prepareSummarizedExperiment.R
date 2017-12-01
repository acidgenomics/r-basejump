context("prepareSummarizedExperiment")

# Create the matrix with invalid names. We'll sanitize these
# into snake_case later.
mat <- matrix(
    seq(1L:16L),
    nrow = 4L,
    ncol = 4L,
    dimnames = list(
        c("ENSMUSG00000000001",
          "ENSMUSG00000000003",
          "ENSMUSG00000000028",
          "ENSMUSG00000000031"),
        c("sample_1",
          "sample_2",
          "sample_3",
          "sample_4")))
# Check handling of rowData (annotable) mismatch
rowdata <- data.frame(
    ensgene = c(
        "ENSMUSG00000000001",
        "ENSMUSG00000000003",
        "ENSMUSG00000000028",
        "ENSMUSG00000000031"),
    biotype = c(
        "coding",
        "coding",
        "coding",
        "coding"),
    row.names = c(
        "ENSMUSG00000000001",
        "ENSMUSG00000000003",
        "ENSMUSG00000000028",
        "ENSMUSG00000000031"))
coldata <- data.frame(
    genotype = c(
        "wildtype",
        "wildtype",
        "knockout",
        "knockout"),
    age = c(3L, 6L, 3L, 6L),
    row.names = colnames(mat))
se <- prepareSummarizedExperiment(
    assays = list(assay = mat),
    rowData = rowdata,
    colData = coldata)

test_that("Valid SummarizedExperiment", {
    expect_equal(
        dim(se),
        c(4L, 4L))
    expect_equal(
        names(metadata(se)),
        c("date",
          "wd",
          "utilsSessionInfo",
          "devtoolsSessionInfo"))
})

test_that("rowData missing", {
    norowdata <- prepareSummarizedExperiment(
        assays = list(assay = mat),
        colData = coldata)
    emptydf <- DataFrame(row.names = seq(1L:4L))
    rownames(emptydf) <- NULL
    expect_equal(
        slot(norowdata, "elementMetadata"),
        emptydf
    )
    # Rownames should still be set though
    expect_equal(
        slot(norowdata, "NAMES"),
        c("ENSMUSG00000000001",
          "ENSMUSG00000000003",
          "ENSMUSG00000000028",
          "ENSMUSG00000000031")
    )
})

test_that("colData missing", {
    expect_error(
        prepareSummarizedExperiment(assays = list(assay = mat)),
        "argument \"colData\" is missing, with no default"
    )
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = mat),
            colData = NULL),
        "colData is required"
    )
})

test_that("Ensure 'assays()' requires a list", {
    expect_error(
        prepareSummarizedExperiment(
            assays = mat,
            rowData = rowdata,
            colData = coldata),
        paste("unable to find an inherited method for function",
              "'prepareSummarizedExperiment' for signature '\"matrix\"'"))
})

# This checks to see if there are any dashes in the names
test_that("Enforce strict names", {
    matbadrows <- mat
    rownames(matbadrows) <- paste0(rownames(matbadrows), "-XXX")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = matbadrows),
            rowData = rowdata,
            colData = coldata),
        "Rownames are not valid")
    matbadcols <- mat
    colnames(matbadcols) <- paste0(colnames(matbadcols), "-XXX")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = matbadcols),
            rowData = rowdata,
            colData = coldata),
        "Colnames are not valid")
})

test_that("Duplicate names", {
    matduperows <- mat
    rownames(matduperows) <- c(
        "ENSMUSG00000000001",
        "ENSMUSG00000000001",
        "ENSMUSG00000000003",
        "ENSMUSG00000000003")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = matduperows),
            rowData = rowdata,
            colData = coldata),
        "Non-unique rownames")
    matdupecols <- mat
    colnames(matdupecols) <- c(
        "sample_1",
        "sample_1",
        "sample_2",
        "sample_2")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = matdupecols),
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
            assays = list(assay = mat),
            rowData = rowdata,
            colData = c(xxx = "yyy")),
        "colData must support 'dim\\(\\)'")
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = mat),
            rowData = c(xxx = "yyy"),
            colData = coldata),
        "rowData must support 'dim\\(\\)'")
})

test_that("Invalid metadata", {
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = mat),
            rowData = rowdata,
            colData = coldata,
            metadata = Sys.Date()),
        "Metadata must be 'list' class object")
})

test_that("dimension mismatch", {
    matextracol <- cbind(mat, "sample_5" = seq(17L, 20L))
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = matextracol),
            rowData = rowdata,
            colData = coldata),
        "Sample mismatch detected")
    matextrarow <- rbind(mat, "ENSMUSG00000000037" = seq(17L, 20L))
    expect_warning(
        prepareSummarizedExperiment(
            assays = list(assay = matextrarow),
            rowData = rowdata,
            colData = coldata),
        "Unannotated genes detected in counts matrix")
    seextrarow <- suppressWarnings(prepareSummarizedExperiment(
        assays = list(assay = matextrarow),
        rowData = rowdata,
        colData = coldata
    ))
    expect_equal(
        metadata(seextrarow)[["unannotatedGenes"]],
        "ENSMUSG00000000037"
    )
})

test_that("Missing rownames", {
    # Missing rownames
    matnorownames <- mat
    rownames(matnorownames) <- NULL
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = matnorownames),
            rowData = rowdata,
            colData = coldata),
        "Assay missing rownames")
    matnocolnames <- mat
    colnames(matnocolnames) <- NULL
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = matnocolnames),
            rowData = rowdata,
            colData = coldata),
        "Assay missing colnames")
    rowdatanorownames <- rowdata
    rownames(rowdatanorownames) <- NULL
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = mat),
            rowData = rowdatanorownames,
            colData = coldata),
        "rowData missing rownames")
    coldatanorownames <- coldata
    rownames(coldatanorownames) <- NULL
    expect_error(
        prepareSummarizedExperiment(
            assays = list(assay = mat),
            rowData = rowdata,
            colData = coldatanorownames),
        "colData missing rownames")
})

test_that("Deprecated functions", {
    expect_warning(
        packageSE(
            assays = list(assay = mat),
            colData = coldata,
            rowData = rowdata),
        "Use 'prepareSummarizedExperiment' instead.")
    expect_warning(
        prepareSE(
            assays = list(assay = mat),
            colData = coldata,
            rowData = rowdata),
        "Use 'prepareSummarizedExperiment' instead.")
})
