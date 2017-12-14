extdataDir <- file.path("inst", "extdata")
dir.create(extdataDir, recursive = TRUE, showWarnings = FALSE)

vector <- c(
    "hello world",
    "HELLO WORLD",
    "RNAi clones",
    "nCount",
    "tx2gene",
    "TX2GeneID",
    "G2M.Score",
    "worfdbHTMLRemap",
    "Mazda RX4",
    123L,
    NA)

namedVector <- c(
    "Item.A" = "hello world",
    "Item.B" = "HELLO WORLD")

factor <- factor(
    c("sample 1" = "group 1",
      "sample 2" = "group 1",
      "sample 3" = "group 2",
      "sample 4" = "group 2"))

dataFrame <- head(mtcars)

matrix <- as.matrix(dataFrame)

tibble <- head(starwars)

list <- list(
    "Item.A" = c(1L, 2L),
    "Item.B" = c(3L, 4L))

makeNames <- list(
    "vector" = vector,
    "namedVector" = namedVector,
    "factor" = factor,
    "dataFrame" = dataFrame,
    "matrix" = matrix,
    "tibble" = tibble,
    "list" = list)
lapply(makeNames, class)

save(makeNames,
     file = file.path(extdataDir, "makeNames.rda"),
     compress = "xz")
