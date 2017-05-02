#' DNA sequence utilites
#'
#' @author Michael Steinbaugh
#'
#' @param dna DNA sequence (ATGC nucleotides)
#'
#' @return Modified DNA sequence



#' @rdname dna
#' @description Complement DNA sequence
#' @export
#' @examples
#' comp("ATGCATGC")
comp <- function(dna) {
    dna <- toupper(dna)
    comp <- dna %>%
        # AT base pair swap
        gsub("A", "A1", .) %>%
        gsub("T", "A", .) %>%
        gsub("A1", "T", .) %>%
        # GC base pair swap
        gsub("G", "G1", .) %>%
        gsub("C", "G", .) %>%
        gsub("G1", "C", .)
    return(comp)
}



#' @rdname dna
#' @description Reverse complement DNA sequence
#' @export
#' @examples
#' revcomp("ATGCATGC")
revcomp <- function(dna) {
    dna <- toupper(dna)
    comp <- comp(dna)
    revcomp <- strsplit(comp, "")[[1]] %>%
        .[order(seq_along(.), decreasing = TRUE)] %>%
        paste(., sep = "", collapse = "")
    return(revcomp)
}
