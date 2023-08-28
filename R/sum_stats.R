sum_stats <- function(long_df) {
  require("dplyr")
  sum_stat<- long_df %>% group_by(Sample) %>% summarise(
    IDs = n(),
    Mean = mean(Intensity),
    Median = median(Intensity),
    SD = sd(Intensity),
    Var = var(Intensity)) %>% ungroup()
  return(sum_stat)
}
