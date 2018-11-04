# Legacy support for S4 coercion methods.
# Safe to remove in a future update, once depency packages are updated.



#' @name as
#' @inherit basejump.coercion::as
NULL



# DataFrame ====================================================================
#' @rdname as
#' @name coerce,sparseMatrix,DataFrame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)

#' @rdname as
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



# data.frame ===================================================================
#' @rdname as
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)



# tibble =======================================================================
#' @rdname as
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)

#' @rdname as
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)
