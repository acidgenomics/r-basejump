library(goseq)

#' Execute goseq
#'
#' Allows for input of differentially expressed genes (DEG),
#' background genes (bg), output environemnt, and data save folder.
#'
#' @param deg
#' @param bg
#' @param envir
#' @param save_dir
#'
#' @return
#' @export
#'
#' @examples
goseq_exec <- function(deg,
                       bg,
                       envir = .GlobalEnv,
                       save_dir = ".") {
  head(deg)
  GO_input <- as.integer(bg %in% deg)
  names(GO_input) <- bg

  # Fit the probability weighting function and plot the resulting fit
  pdf(file.path(save_dir, "pwf.pdf"))
  pwf <- nullp(GO_input, genome, gene_format, bias.data = transcript_lengths)
  dev.off()

  # Wallenius approximation
  # `use_genes_without_cat = TRUE` allows unannotated genes in the analysis
  GO_wallenius <- goseq(pwf, genome, gene_format)
  # KEGG doesn't find a match for most genes with GeneID...
  # Use ORF format instead? e.g. CELE_Y110A7A.10
  KEGG_wallenius <- goseq(pwf, genome, gene_format, test.cats = "KEGG")

  # Find GO to gene symbol matches, only need to do this once for the data set.
  GO_gene2cat <- getgo(rownames(pwf), genome, gene_format)
  KEGG_gene2cat <- getgo(rownames(pwf), genome, gene_format, fetch.cats = "KEGG")

  GO_tmp <- unlist(GO_gene2cat, use.names = FALSE)
  KEGG_tmp <- unlist(KEGG_gene2cat, use.names = FALSE)

  names(GO_tmp) <- rep(names(GO_gene2cat), times = as.numeric(summary(GO_gene2cat)[, 1]))
  names(KEGG_tmp) <- rep(names(KEGG_gene2cat), times = as.numeric(summary(KEGG_gene2cat)[, 1]))

  GO_cat2gene <- split(names(GO_tmp), as.vector(GO_tmp))
  KEGG_cat2gene <- split(names(KEGG_tmp), as.vector(KEGG_tmp))

  # Add adjusted p-value
  GO_data <- GO_wallenius
  KEGG_data <- KEGG_wallenius

  GO_data$over_represented_pvalue_BH <- p.adjust(GO_data$over_represented_pvalue, method = "BH")
  KEGG_data$over_represented_pvalue_BH <- p.adjust(KEGG_data$over_represented_pvalue, method = "BH")

  GO_data$under_represented_pvalue <- NULL
  KEGG_data$under_represented_pvalue <- NULL

  # Query by adjusted p-value, get a smaller list
  GO_table <- GO_data[which(GO_data[, "over_represented_pvalue_BH"] <= 0.05), ]
  KEGG_table <- KEGG_data[which(KEGG_data[, "over_represented_pvalue_BH"] <= 0.05), ]

  # Add KEGG term names to table
  KEGG_table$term <- unlist(mget(KEGG_table$category, KEGGPATHID2NAME, ifnotfound = NA))

  # getgo outputs uppercase names in cat2gene, need to convert first
  deg_upper <- toupper(deg)

  GO_genes <- lapply(GO_table$category, function(x) sort(intersect(unlist(GO_cat2gene[x]), deg_upper)))
  KEGG_genes <- lapply(KEGG_table$category, function(x) sort(intersect(unlist(KEGG_cat2gene[x]), deg_upper)))

  GO_glist <- lapply(GO_genes, function(x) paste(x, collapse = ', '))
  KEGG_glist <- lapply(KEGG_genes, function(x) paste(x, collapse = ', '))

  GO_table$genes <- unlist(GO_glist)
  KEGG_table$genes <- unlist(KEGG_glist)

  # Switch the case back to proper spelling
  GO_table$genes <- gsub("WBGENE", "WBGene", GO_table$genes)
  KEGG_table$genes <- gsub("WBGENE", "WBGene", KEGG_table$genes)

  assign(paste0(contrast_name, "_goseq_GO_", goseq_runs[x]),
         GO_table,
         envir = envir)
  assign(paste0(contrast_name, "_goseq_KEGG_", goseq_runs[x]),
         KEGG_table,
         envir = envir)
}
