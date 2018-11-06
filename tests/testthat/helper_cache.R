invisible(lapply(
    X = c(
        "gr.rda",
        "mn.rda"
    ),
    FUN = function(file, url) {
        if (!file.exists(file)) {
            utils::download.file(
                url = paste(url, file, sep = "/"),
                destfile = file
            )
        }
    },
    url = basejump.globals::basejumpCacheURL
))
