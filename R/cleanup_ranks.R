cleanup_ranks <- function(input) {
  ba_ranks <- input[,2]
  for (i in seq.int(9, ncol(input)-2, 6)) {
    ba_ranks[ncol(ba_ranks)+1] <- input[i]
  }
  colnames(ba_ranks) <- c(input[2,2], input[1,(seq.int(4,ncol(input)-7,6))])
  ba_ranks <- ba_ranks[-(1:2),]
  return(ba_ranks)
}
