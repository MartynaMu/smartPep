maxlfq <- function(data_file) {
  library("tidyverse")
  dfRaw <- read.delim(data_file)
  dfRaw <- filter(dfRaw, !grepl("Biognosys", dfRaw$Protein.Ids))
  dfRaw <- filter(dfRaw, !grepl("contam", dfRaw$Protein.Ids))

  peps_maxlfq <- diann::diann_maxlfq(dfRaw[dfRaw$Q.Value <= 0.01 & dfRaw$PG.Q.Value <= 0.01,],
                                     group.header="Modified.Sequence", id.header = "Precursor.Id",
                                     quantity.header = "Precursor.Normalised")

  maxLFQ_ip <- as.data.frame(peps_maxlfq)

  maxLFQ_ip <- tibble::rownames_to_column(maxLFQ_ip, "Modified.Sequence")

  add <- dfRaw |> select(Protein.Ids, Genes, Stripped.Sequence, Modified.Sequence)

  id <- add %>% group_by(Protein.Ids, Genes, Stripped.Sequence) %>%
    reframe(Modified.Sequence = unique(Modified.Sequence))

  df_new <- id |> left_join(y = maxLFQ_ip, by = "Modified.Sequence")
  df_new <- as.data.frame(df_new)
  return(df_new)
}
