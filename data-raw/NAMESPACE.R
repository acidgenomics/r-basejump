# Need to quote functions containing `%>%`, `<-`.

# Export capitalized, upper camel functions first.
Sys.setlocale("LC_COLLATE", "C")

ns <- getNamespaceInfo(ns = "basejump", which = "imports")
# Get only the named entries in list.
ns <- ns[which(names(ns) != "")]
# Remove `.__` entries.
ns <- lapply(
    X = ns,
    FUN = function(x) {
        x[!grepl("^.__", x)]
    }
)
ns <- unlist(ns)
ns <- sort(unique(as.character(ns)))

con <- stdout()
# con <- file("NAMESPACE")
text <- paste0("export(", ns, ")")
writeLines(text = text, con = con)
# close(con)
