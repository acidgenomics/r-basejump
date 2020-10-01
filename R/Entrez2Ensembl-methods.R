## Updated 2020-10-01.
`Entrez2Ensembl,character` <-
    function(
        object,
        organism,
        format = c("1:1", "long"),
        strict = TRUE
    ) {
        assert(isFlag(strict))
        format <- match.arg(format)
        df <- select(
            x = .ncbiOrgDb(organism),
            keys = object,
            keytype = "ENTREZID",
            columns = "ENSEMBL", "SYMBOL"
        )

        ## FIXME STRICT MATCH MODE.

        ## FIXME RETURN 1:1 HERE.

        df
    }



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Entrez2Ensembl",
    signature = signature("character"),
    definition = `Entrez2Ensembl,character`
)
