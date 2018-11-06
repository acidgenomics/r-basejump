#' Access to the Data Inside an S4 Method Definition
#'
#' @name MethodDefinition
#'
#' @inheritParams methods::selectMethod
#' @inheritParams methods::setMethod
#' @param package `string`. Package name.
#'
#' @return
#' - [methodFormals()]: `list`. Extract the sealed formal arguments.
#' - [methodFunction()]: `function`. Extract the sealed function.
#'
#' @seealso
#' - Modified version of [John Chambers' code](https://goo.gl/ymX571).
#' - `help(topic = "MethodDefinition-class", package = "methods")`.
#' - [methods::selectMethod()], [methods::setMethod()].
#'
#' @examples
#' library(methods)
#' library(BiocGenerics)
#'
#' f <- "as.data.frame"
#' signature <- "ANY"
#'
#' ## Function.
#' x <- methodFunction(f = f, signature = signature)
#' class(x)
#' formals(x)
#'
#' ## Formals.
#' x <- methodFormals(f = f, signature = signature)
#' class(x)
#' print(x)
NULL



#' @rdname MethodDefinition
#' @export
methodFunction <- function(f, signature, package) {
    if (missing(package) || is.null(package)) {
        envir <- NULL
    } else {
        assert_is_a_string(package)
        envir <- asNamespace(package)
    }

    # Locate the S4 generic. We're opting to get either the `standardGeneric` or
    # the `nonstandardGenericFunction` instead of requiring `standardGeneric`
    # via `getGeneric()` here, since it's more flexible with re-exported generic
    # functions.
    args <- Filter(
        f = Negate(is.null),
        x = list(
            x = f,
            envir = envir,
            inherits = TRUE
        )
    )
    generic <- tryCatch(
        expr = do.call(what = get, args = args),
        error = function(e) {
            stop(paste("Failed to locate", f, "generic."))
        }
    )
    # Assert that we're getting an S4 generic.
    assert_that(isS4(generic))
    args <- Filter(
        f = Negate(is.null),
        x = list(
            f = f,
            where = envir,
            getName = FALSE
        )
    )
    assert_that(do.call(what = isGeneric, args = args))

    # Now select the method from the generic.
    definition <- selectMethod(
        f = generic,
        signature = signature,
        useInherited = TRUE,
        doCache = FALSE
    )
    assert_that(is(definition, "MethodDefinition"))

    # S4 dispatch will nest `.local()` function inside the method definition
    # when the formals aren't identical to the generic. Otherwise it will be
    # slotted in ".Data".
    if (isTRUE(hasLocal(definition))) {
        fun <- extractLocal(definition)
    } else {
        fun <- slot(definition, ".Data")
    }
    assert_is_function(fun)

    fun
}



#' @rdname MethodDefinition
#' @export
methodFormals <- function(f, signature, package) {
    if (missing(package)) {
        package <- NULL
    }
    definition <- methodFunction(
        f = f,
        signature = signature,
        package = package
    )
    formals(definition)
}



#' @rdname MethodDefinition
#' @export
hasLocal <- function(definition) {
    assert_that(is(definition, "MethodDefinition"))
    assert_is_function(definition)
    body <- body(definition)
    if (!is(body, "{")) {
        return(FALSE)
    }
    if (!is(body[[2L]], "<-")) {
        return(FALSE)
    }
    if (!identical(body[[2L]][[2L]], as.name(".local"))) {
        return(FALSE)
    }
    TRUE
}



#' @rdname MethodDefinition
#' @export
extractLocal <- function(definition) {
    assert_that(hasLocal(definition))
    body <- body(definition)
    local <- eval(body[[2L]][[3L]])
    assert_is_function(local)
    local
}
