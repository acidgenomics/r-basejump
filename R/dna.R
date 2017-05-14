#' DNA sequence utilites.
#'
#' @param dna DNA sequence (ATGC nucleotides).
#'
#' @return Modified DNA sequence.



#' @rdname dna
#' @description Complement DNA sequence.
#' @export
#' @examples
#' comp("ATGCATGC")
comp <- function(dna) {
    dna <- toupper(dna)
    comp <- dna %>%
        # AT base pair swap
        str_replace_all("A", "A1") %>%
        str_replace_all("T", "A") %>%
        str_replace_all("A1", "T") %>%
        # GC base pair swap
        str_replace_all("G", "G1") %>%
        str_replace_all("C", "G") %>%
        str_replace_all("G1", "C")
    return(comp)
}



#' @rdname dna
#' @description Reverse complement DNA sequence.
#' @export
#' @examples
#' revcomp("ATGCATGC")
revcomp <- function(dna) {
    dna <- toupper(dna)
    comp <- comp(dna)
    revcomp <- str_split(comp, "")[[1]] %>%
        .[order(seq_along(.), decreasing = TRUE)] %>%
        paste(sep = "", collapse = "")
    return(revcomp)
}
