hm_small <- function(wide_df, genePattern, cols) {
  hmT <- wide_df
  hmT <- replace(wide_df, is.na(wide_df), 0)
  hmT$index <- paste(wide_df$Genes, 1:nrow(wide_df), sep = ".")
  hmTsmall <- dplyr::filter(hmT, grepl(genePattern, hmT$index))
  rownames(hmTsmall) <- hmTsmall$index
  hmTsmall <- scrime::rowScales(hmTsmall[cols])
  hmTsmall <- tidyr::drop_na(hmTsmall)
  ComplexHeatmap::Heatmap(as.matrix(hmTsmall),
          show_row_dend = FALSE,
          heatmap_legend_param = list(title = "Row z-score"),
          clustering_distance_columns = "pearson", clustering_method_columns = "ward",
          clustering_distance_rows = "pearson",
    )
}
