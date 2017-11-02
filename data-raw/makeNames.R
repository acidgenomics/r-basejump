vec <- c(
    "hello world",
    "HELLO WORLD",
    "RNAi clones",
    "nCount",
    "tx2gene",
    "TX2GeneID",
    "G2M.Score",
    "worfdbHTMLRemap",
    "Mazda RX4",
    123,
    NA)

namedVec <- c(
    Item.A = "hello world",
    Item.B = "HELLO WORLD")

df <- head(mtcars)

mat <- as.matrix(df)

tbl <- head(starwars)

lst <- list(
    Item.A = c(1, 2),
    Item.B = c(3, 4))

makeNames <- list(
    vec = vec,
    namedVec = namedVec,
    df = df,
    mat = mat,
    tbl = tbl,
    lst = lst
)

save(makeNames,
     file = "~/Desktop/makeNames.rda",
     compress = "xz")
