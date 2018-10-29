imports <- getNamespaceInfo(ns = "basejump", which = "imports")
# Get only the named entries in list.
imports <- imports[which(names(imports) != "")]
# Remove `.__` entries.
imports <- lapply(
    X = imports,
    FUN = function(x) {
        x[!grepl("^.__", x)]
    }
)
unlist <- unlist(imports)

# Look for duplicates.


# getNamespaceExports("basejump.annotations")
# funs <- ls(getNamespaceInfo(ns = "basejump.annotations", which = "exports"))
