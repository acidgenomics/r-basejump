set.seed(1L)
invisible(lapply(
    X = c("testthat", "patrick", "basejump"),
    FUN = library,
    character.only = TRUE
))
test_check("basejump")
