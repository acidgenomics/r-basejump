library(readr)

# Download the PANTHER files and keep only the first 10 lines.
pattern <- .pantherMappings %>%
    as.character() %>%
    paste0(collapse = "|")
files <- transmit(
    remoteDir = paste(
        "ftp://ftp.pantherdb.org",
        "sequence_classifications",
        "current_release",
        "PANTHER_Sequence_Classification_files",
        sep = "/"
    ),
    pattern = pattern,
    compress = FALSE
)
panther <- lapply(
    X = files,
    FUN = function(file) {
        x <- read_lines(file, n_max = 10L)
        write_lines(x = x, path = basename(file))
        gzip(basename(file), overwrite = TRUE)
        x
    }
)
names(panther)

# Match the test HGNC IDs to minimal PANTHER data.
header <- read_lines(.hgncURL, n_max = 1L)
lines <- read_lines(.hgncURL, skip = 1L)
ids <- panther %>%
    .[[grep("human", names(.))]] %>%
    str_match("HGNC=([[:digit:]]+)") %>%
    .[, 1] %>%
    gsub("=", ":", .)
hgnc <- vapply(
    X = ids,
    FUN = function(id) {
        grep(
            pattern = paste0("^", id, "\t"),
            x = lines,
            value = TRUE
        )
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
)
hgnc <- c(header, hgnc)
write_lines(hgnc, "hgnc.txt")
gzip("hgnc.txt", overwrite = TRUE)

# Match the test MGI IDs to minimal PANTHER data.
# mgi2ensembl
header <- read_lines(.mgiURL, n_max = 1L)
lines <- read_lines(.mgiURL, skip = 1L)
ids <- panther %>%
    .[[grep("mouse", names(.))]] %>%
    str_match("MGI=([[:digit:]]+)") %>%
    .[, 1] %>%
    gsub("=", ":", .)
mgi <- vapply(
    X = ids,
    FUN = function(id) {
        grep(
            pattern = paste0("^", id, "\t"),
            x = lines,
            value = TRUE
        )
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
)
mgi <- c(header, mgi)
write_lines(mgi, "mgi.rpt")
gzip("mgi.rpt", overwrite = TRUE)
