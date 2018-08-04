# Sanitize formals into snake case and abort on duplicates.
# Duplicates may arise if user is mixing and matching camel/snake case.
.pheatmapArgs <- function(args) {
    assert_is_list(args)
    assert_has_names(args)
    # Abort on snake case formatted formalArgs
    invalidNames <- grep("[._]", names(args), value = TRUE)
    if (length(invalidNames)) {
        stop(paste(
            "Define formalArgs in camel case:",
            toString(invalidNames)
        ))
    }
    names(args) <- snake(names(args))
    assert_is_subset(names(args), formalArgs(pheatmap))
    args
}



# Automatically handle the annotation columns.
# Factors with a single level are automatically dropped.
.pheatmapAnnotationCol <- function(data) {
    # pheatmap requires `NA` argument if empty
    if (!has_dims(data)) {
        return(NA)
    }
    assertHasRownames(data)
    blacklist <- c("sampleName", metadataBlacklist)

    data <- data %>%
        as.data.frame() %>%
        # Remove sample name columns
        .[, setdiff(colnames(.), blacklist), drop = FALSE] %>%
        rownames_to_column() %>%
        # Ensure all strings are factor
        mutate_if(is.character, as.factor) %>%
        # Ensure unwanted columns like `sizeFactor` are dropped
        select_if(is.factor) %>%
        column_to_rownames()

    # Drop any remaining factor columns that contain a single level
    hasLevels <- vapply(
        data,
        FUN = function(x) {
            length(levels(x)) > 1L
        },
        FUN.VALUE = logical(1L)
    )

    if (!length(hasLevels)) {
        return(NA)  # nocov
    }

    data[, hasLevels, drop = FALSE]
}



# Define colors for each annotation column
.pheatmapAnnotationColors <- function(annotationCol, legendColor) {
    assertIsHexColorFunctionOrNULL(legendColor)
    if (is.data.frame(annotationCol) && is.function(legendColor)) {
        list <- lapply(
            X = annotationCol,
            FUN = function(col) {
                assert_is_factor(col)
                levels <- levels(col)
                colors <- legendColor(length(levels))
                names(colors) <- levels
                colors
            })
        names(list) <- colnames(annotationCol)
        list
    } else {
        NA
    }
}



# If `color = NULL`, use the pheatmap default palette
.pheatmapColor <- function(color = NULL, n = 256L) {
    if (is.character(color)) {
        # Hexadecimal color palette
        # (e.g. RColorBrewer palettes)
        assert_all_are_hex_colors(color)
        color
    } else if (is.function(color)) {
        # Hexadecimal color function
        # (e.g. viridis functions)
        assertIsHexColorFunctionOrNULL(color)
        color(n)
    } else {
        # pheatmap default palette
        colorRampPalette(rev(brewer.pal(n = 7L, name = "RdYlBu")))(n)
    }
}
