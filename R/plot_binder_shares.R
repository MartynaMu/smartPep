plot_binder_shares <- function(ba_long, MHC_col, category_col) {
  require("ggplot2")
  theme_set(DEP::theme_DEP1())

  all_alleles = filter(ba_long, ba_long[[MHC_col]] != "Any HLA allele")
  any_allele = filter(ba_long, ba_long[[MHC_col]] == "Any HLA allele")

  plot_elements = geom_bar(aes(x = get(MHC_col),
                               fill = get(category_col)),
                           width = .5,
                           position = "fill")
  scale = scale_fill_grey(start = 0.8, end = 0.2)
  labels = labs(y = "Share", x = MHC_col, fill = category_col)

  a <- ggplot(all_alleles)+
    plot_elements+
    scale+
    labels

  b <- ggplot(any_allele)+
    plot_elements+
    scale+
    labels

  c <- ggpubr::ggarrange(a, b, ncol = 2, widths = c(1, .6))
  return(c)
}
