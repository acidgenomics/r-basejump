# Make Names Sanitization Examples
# Last updated 2018-08-27

character <- c(
    "hello world",
    "HELLO WORLD",
    "RNAi clones",
    "nCount",
    "tx2gene",
    "TX2GeneID",
    "G2M.Score",
    "worfdbHTMLRemap",
    "Mazda RX4",
    "%GC",
    "5prime",
    "5'-3' bias",
    123L,
    NA
)

namedCharacter <- c(
    Item.A = "hello world",
    Item.B = "HELLO WORLD"
)

factor <- factor(
    c(
        "sample 1" = "group 1",
        "sample 2" = "group 1",
        "sample 3" = "group 2",
        "sample 4" = "group 2"
    )
)

dataFrame <- head(datasets::USArrests)
matrix <- as.matrix(dataFrame)
tibble <- head(ggplot2::mpg)

list <- list(
    Item.A = c(1L, 2L),
    Item.B = c(3L, 4L)
)

mn <- list(
    character = character,
    namedCharacter = namedCharacter,
    factor = factor,
    dataFrame = dataFrame,
    matrix = matrix,
    tibble = tibble,
    list = list
)

saveData(mn, dir = "tests/testthat", compress = "xz")
