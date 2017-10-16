vec <- c(
    "hello world",
    "HELLO WORLD",
    "RNAi clones",
    "tx2gene",
    "TX2GeneID",
    "G2M.Score",
    "worfdbHTMLRemap",
    123,
    NA)

namedVec <- c(
    Item.A = "hello world",
    Item.B = "HELLO WORLD")

df <- head(mtcars)

tbl <- head(starwars)

lst <- list(
    Item.A = c(1, 2),
    Item.B = c(3, 4))

makeNames <- list(
    vec = vec,
    namedVec = namedVec,
    df = df,
    tbl = tbl,
    lst = lst
)

save(makeNames,
     file = file.path(testDataDir, "makeNames.rda"),
     compress = "xz")
