#' Humanize an R Object
#'
#' @note This can make dimnames invalid (see `make.names()`) and should only
#' be called prior to writing files to disk.
#'
#' @name humanize
#' @inheritParams params
#'
#' @return Modified object, with human-friendly rownames (e.g. gene symbols
#'   instead of stable gene IDs) and colnames (e.g. sample names instead of
#'   sample IDs).
NULL



