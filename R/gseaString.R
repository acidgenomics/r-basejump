#' Gene set enrichment analysis (GSEA) with STRING
#'
#' @import STRINGdb
#'
#' @param gene Gene identifier vector.
#' @param background Background identifier vector.
#' @param species Species (e.g. "Homo sapiens")
#' @param cache Cache files to disk (TRUE/FALSE).
#' @param csv Export to CSV (TRUE/FALSE).
#' @param png Export network as PNG file (TRUE/FALSE).
#' @param pdf Export network as high resolution PDF file (TRUE/FALSE).
#' @param version STRING version to use.
#'
#' @return STRING list.
#' @export
gseaString <- function(gene,
                       background,
                       species,
                       cache = TRUE,
                       csv = FALSE,
                       png = TRUE,
                       pdf = FALSE,
                       version = "10") {
    if (isTRUE(cache)) {
        cache <- "cache/string"
        if (!file.exists(cache)) {
            dir.create(cache, recursive = TRUE)
        }
    }

    if (isTRUE(csv) | isTRUE(png) | isTRUE(pdf)) {
        if (is.null(getOption("exportDir"))) {
            stop("exportDir must be declared in options().")
        }
        stringDir <- file.path(getOption("exportDir"), "string")
        if (!file.exists(stringDir)) {
            dir.create(stringDir, recursive = TRUE)
        }
    }

    species <- STRINGdb::get_STRING_species(version = version) %>%
        .[.$official_name == get("species"), "species_id"]
    string_db <- STRINGdb::STRINGdb$new(species = species,
                                        input_directory = cache,
                                        version = version)
    backgroundV <- background %>% string_db$mp(.)
    string_db <- STRINGdb::STRINGdb$new(species = species,
                                        input_directory = cache,
                                        backgroundV = backgroundV,
                                        version = version)
    id <- gene %>% string_db$mp(.)

    string <-
        list(id = id,
             backgroundV = backgroundV,
             geneOntologyBiologicalProcess = string_db$get_enrichment(id, category = "Process"),
             geneOntologyMolecularFunction = string_db$get_enrichment(id, category = "Function"),
             geneOntologyCellularComponent = string_db$get_enrichment(id, category = "Component"),
             kegg = string_db$get_enrichment(id, category = "KEGG"),
             interpro = string_db$get_enrichment(id, category = "InterPro"),
             pfam = string_db$get_enrichment(id, category = "Pfam"))

    if (isTRUE(csv)) {
        readr::write_csv(string$geneOntologyBiologicalProcess, file.path(stringDir, "geneOntologyBiologicalProcess.csv"))
        readr::write_csv(string$geneOntologyMolecularFunction, file.path(stringDir, "geneOntologyMolecularFunction.csv"))
        readr::write_csv(string$geneOntologyCellularComponent, file.path(stringDir, "geneOntologyCellularComponent.csv"))
        readr::write_csv(string$kegg, file.path(stringDir, "kegg.csv"))
        readr::write_csv(string$interpro, file.path(stringDir, "interpro.csv"))
        readr::write_csv(string$pfam, file.path(stringDir, "pfam.csv"))
    }

    if (isTRUE(png)) {
        string_db$get_png(id, file = file.path(stringDir, "string.png"))
    }

    # STILL IN DEVELOPMENT:
    if (isTRUE(pdf)) {
        # pdf(file.path(stringDir, "string.pdf"))
        # string_db$plot_network(id, add_link = FALSE)
        # dev.off()
    }

    return(string)
}
