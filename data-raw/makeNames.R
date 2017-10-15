unnamedVec <- c(
    "hello world",
    "HELLO WORLD",
    "RNAi clones",
    "worfdbHTMLRemap",
    123,
    NA)
namedVec <- c(
    Item.A = "hello world",
    Item.B = "HELLO WORLD")
df <- head(mtcars)
lst <- list(
    Item.A = c(1, 2),
    Item.B = c(3, 4))

makeNames <- list(
    unnamedVec = unnamedVec,
    namedVec = namedVec,
    df = df,
    lst = lst
)
save(makeNames,
     file = file.path(testDataDir, "makeNames.rda"),
     compress = "xz")
