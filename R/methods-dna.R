#' DNA Sequence Utilites
#'
#' Generate complement or reverse complement sequences.
#'
#' @rdname dna
#' @name dna
#' @family DNA Sequence Utilities
#'
#' @inheritParams AllGenerics
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
        if (!str_detect(object, "^[ACGT]+$")) {
            stop("DNA string must only contain ACGT nucleotides",
                 call. = FALSE)
        }
        object %>%
            toupper() %>%
            # AT base pair swap
            str_replace_all("A", "A1") %>%
            str_replace_all("T", "A") %>%
            str_replace_all("A1", "T") %>%
            # GC base pair swap
            str_replace_all("G", "G1") %>%
            str_replace_all("C", "G") %>%
            str_replace_all("G1", "C")
    })



#' @rdname dna
#' @export
setMethod(
    "revcomp",
    signature("character"), function(object) {
        object <- toupper(object)
        comp <- comp(object)
        revcomp <- str_split(comp, "") %>%
            .[[1L]] %>%
            .[order(seq_along(.), decreasing = TRUE)] %>%
            paste0(collapse = "")
        revcomp
    })
