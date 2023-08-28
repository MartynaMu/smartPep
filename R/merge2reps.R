merge2reps <- function(wide_df) {
  intensity_indices <- which(sapply(wide_df,is.numeric))
  e = 1
  for (i in seq.int(intensity_indices[1], max(intensity_indices), 2)) { #seq from 1st num col to the last one by every 2
    wide_df[, i] <- if_else( #pasting value from next tech rep in case of NA
      is.na(wide_df[, i]) == TRUE,
      wide_df[, i+1],
      wide_df[, i])
    wide_df[, i+1] <- if_else( #pasting a value from previous tech rep in case of NA
      is.na(wide_df[, i+1]) == TRUE,
      wide_df[, i],
      wide_df[, i+1])
    new <- rowMeans(wide_df[, c(i, (i+1))]) #creating a new variable with mean result of every 2 cols
    wide_df[,(ncol(wide_df)+1)] <- new #adding the created variable to the df
    colnames(wide_df)[ncol(wide_df)] <- paste0("Sample", e)
    e <- e + 1
  }
  wide_df <- wide_df[,-(intensity_indices)]
  return(wide_df)
}
