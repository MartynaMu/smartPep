binding_category <- function(x) {
  #Required packages
  library("dplyr")
  #Based on BA value, tag the peptide "Strong/Weak/Non- binder" for all alleles (columns)
  cols = 2:ncol(x)
  for (i2 in cols) {
    x[,ncol(x)+1] <- case_when(
      0.5 >= x[,i2] & x[,i2] <= 2 ~ "Strong binder",
      2 >= x[,i2] & x[,i2] >= 0.5 ~ "Weak binder",
      .default = "Non-binder"
    )
    #Change the new column name to the original column name with HLA allele
    colnames(x)[ncol(x)] <- colnames(x)[i2]
  }
  #Subset the newly computed columns
  x <- x[-(cols)]
  #Create a new column for "Any HLA allele" BA category
  x[,ncol(x)+1] <- NA
  colnames(x)[ncol(x)] <- "Any HLA allele"
  #Based on previously computed columns, tag whether the peptide binds to anything or nothing - Binder/Non-binder
  for (i in seq.int(1:nrow(x))) {
    if (sum(match(x[i,], c("Strong binder", "Weak binder")), na.rm = TRUE) >= 1) {
      x[i,ncol(x)] = "Binder"
    } else {
      x[i,ncol(x)] = "Non-binder"
    }
  }
  return(x)
}

