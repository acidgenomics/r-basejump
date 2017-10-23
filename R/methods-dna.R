#' DNA Sequence Utilites
#'
#' Generate complement or reverse complement sequences.
#'
#' @rdname dna
#' @name dna
#' @family DNA Sequence Utilities
#'
#' @inheritParams AllGenerics
#'
#' @param object DNA sequence (`ATGC` nucleotides).
#'
#' @return Matching DNA sequence.
#'
#' @examples
#' dna <- "ATGCATGC"
#' comp(dna)
#' revcomp(dna)
NULL



# Methods ====
#' @rdname dna
#' @export
setMethod(
    "comp",
    signature("character"),
    function(object) {
        if (!grepl(x = object, pattern = "^[ACGT]+$")) {
            stop("DNA string must only contain ACGT nucleotides",
                 call. = FALSE)
        }
        object %>%
            toupper() %>%
            # AT base pair swap
            gsub(x = .,
                 pattern = "A",
                 replacement = "A1") %>%
            gsub(x = .,
                 pattern = "T",
                 replacement = "A") %>%
            gsub(x = .,
                 pattern = "A1",
                 replacement = "T") %>%
            # GC base pair swap
            gsub(x = .,
                 pattern = "G",
                 replacement = "G1") %>%
            gsub(x = .,
                 pattern = "C",
                 replacement = "G") %>%
            gsub(x = .,
                 pattern = "G1",
                 replacement = "C")
    })



#' @rdname dna
#' @export
setMethod(
    "revcomp",
    signature("character"), function(object) {
        object <- toupper(object)
        comp <- comp(object)
        strsplit(comp, split = "", fixed = TRUE) %>%
            unlist() %>%
            .[order(seq_along(.), decreasing = TRUE)] %>%
            paste0(collapse = "")
    })
