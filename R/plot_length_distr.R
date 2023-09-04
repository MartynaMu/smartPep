plot_length_distr <- function(long_df) {
  require("ggplot2")
  long_df$AA.length <- stringr::str_length(long_df$Stripped.Sequence)
  theme_set(DEP::theme_DEP1())
  ggplot(long_df, aes(AA.length)) +
    geom_bar()+
    facet_wrap(~Sample)
}
