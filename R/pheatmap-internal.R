.emptyPheatmapAnnotations <- list(
    annotationCol = NA,
    annotationColors = NA
)



# Automatically handle the annotation data and colors.
# Factors with a single level are automatically dropped.
.pheatmapAnnotations <- function(
    object,
    blacklist = "sampleName",
    legendColor
) {
    validObject(object)
    assert_is_character(blacklist)
    assertIsHexColorFunctionOrNULL(legendColor)

    # Annotation columns -------------------------------------------------------
    data <- colData(object)
    interestingGroups <- interestingGroups(object)

    # pheatmap requires `NA` if empty.
    if (
        !has_dims(data) ||
        !has_length(interestingGroups) ||
        identical(interestingGroups, "sampleName")
    ) {
        return(.emptyPheatmapAnnotations)
    }

    assertHasRownames(data)
    data <- data[, interestingGroups, drop = FALSE]

    # Prepare the blacklist, always excluding sample names from labeling in
    # the pheatmap annotation columns.
    blacklist <- unique(c("sampleName", blacklist))

    data <- data %>%
        as_tibble(rownames = "rowname") %>%
        # Remove blacklisted columns (e.g. `sampleName`).
        .[, setdiff(colnames(.), blacklist), drop = FALSE] %>%
        # Ensure all strings are factors.
        mutate_if(is.character, as.factor) %>%
        # Ensure unwanted columns like `sizeFactor` are dropped.
        select_if(is.factor) %>%
        as.data.frame() %>%
        column_to_rownames("rowname")

    # Drop any remaining factor columns that contain a single level.
    hasLevels <- vapply(
        data,
        FUN = function(x) {
            length(levels(x)) > 1L
        },
        FUN.VALUE = logical(1L)
    )

    # Return empty if there are no useful factor columns.
    if (!has_length(hasLevels)) {
        return(.emptyPheatmapAnnotations)  # nocov
    }

    data <- data[, hasLevels, drop = FALSE]

    # Colors -------------------------------------------------------------------
    if (
        is.data.frame(data) &&
        is.function(legendColor)
    ) {
        colors <- lapply(
            X = data,
            FUN = function(x) {
                assert_is_factor(x)
                levels <- levels(x)
                colors <- legendColor(length(levels))
                names(colors) <- levels
                colors
            })
        names(colors) <- colnames(data)
    } else {
        colors <- NA
    }

    # Return -------------------------------------------------------------------
    list(
        annotationCol = data,
        annotationColors = colors
    )
}



# Sanitize formals into snake case and abort on duplicates.
# Duplicates may arise if user is mixing and matching camel/snake case.
.pheatmapArgs <- function(args) {
    assert_is_list(args)
    assert_has_names(args)
    # Abort on snake case formatted formalArgs
    invalidNames <- grep("[._]", names(args), value = TRUE)
    if (has_length(invalidNames)) {
        stop(paste(
            "Specify arguments in camel case:",
            toString(invalidNames)
        ))
    }
    names(args) <- snake(names(args))
    assert_is_subset(names(args), formalArgs(pheatmap))
    assert_has_no_duplicates(names(args))
    args
}



# If `color = NULL`, use the pheatmap default palette
.pheatmapColorPalette <- function(color = NULL, n = 256L) {
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
