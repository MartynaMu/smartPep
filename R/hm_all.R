hm_all <- function(wide_df, cols) {
  hmT <- wide_df
  hmT <- replace(wide_df, is.na(wide_df), 0)
  hmT$index <- paste(wide_df$Genes, 1:nrow(wide_df), sep = ".")
  hmTbig <- tidyr::drop_na(hmT)
  hmTbig <- scrime::rowScales(hmTbig[cols])
  ComplexHeatmap::Heatmap(as.matrix(drop_na(hmTbig)),
          show_row_dend = FALSE,
          heatmap_legend_param = list(title = "Row z-score"),
          clustering_distance_columns = "pearson", clustering_method_columns = "ward"
  )
}
