#long_df - a long dataframe with columns "Stripped.Sequence" and "Sample"
#sample_col - a string with a name of the column storing sample names
#sequence_col || gene_col - A string column name storing peptide sequences or genes. Either must be provided.
#save - a boolean, whether to save the list in .txt format in dw

pull_ids <- function(long_df, sample_col, sequence_col, gene_col, save) {
  if (missing(gene_col)) {
    id_col = sequence_col
  } else if (missing(sequence_col)) {
    id_col = gene_col
  }
  else {
    stop("Error: provide either sequence_col or gene_col.")
  }
  sample_names <- unique(long_df[[sample_col]])
  ids_list <- list() #create a new list to add sample's identified peptides to
  for (i in sample_names) { #for each sample provided...
    ids = long_df[[id_col]][long_df[[sample_col]] == i]
    ids_list <- append(ids_list, list(ids)) #...add a list of peptides identified to a common list
    if (save == TRUE) { #...and save a .txt file for each sample
      readr::write_lines(x = ids, file = paste(i, id_col, "ids.txt", sep = " "))
    }
  }
  names(ids_list) <- sample_names
  return(ids_list)
}
