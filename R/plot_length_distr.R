plot_length_distr <- function(long_df) {
  require("stringr")
  require("ggplot2")
  dfLLog$AA.length <- str_length(long_df$Stripped.Sequence)
  ggplot(long_df, aes(AA.length)) +
    geom_bar()+
    facet_wrap(~Sample)
}
