# Code for nestedness function

nestedness_s <- function(x){ # x is the adjacency matrix (binary)
  # nestedness of rows
  nested_rows <- 0
  for(i in 1:nrow(x)){
    for(j in i:nrow(x)){
      if(j>i){
        shared <- sum(x[i,]*x[j,]) # sum of common interactions
        min_shared <- min(sum(x[i,]),sum(x[j,])) # min of the degrees
        nested_rows <- nested_rows+(shared/min_shared)
      }
    }
  }

  nestedness_rows <- nested_rows/(nrow(x)*(nrow(x)-1)/2)

  # nestedness of columns
  nested_columns <- 0
  for(i in 1:ncol(x)){
    for(j in i:ncol(x)){
      if(j>i){
        shared <- sum(x[,i]*x[,j]) # sum of common interactions
        min_shared <- min(sum(x[,i]),sum(x[,j])) # min of the degrees
        nested_columns <- nested_columns+(shared/min_shared)
      }
    }
  }


  nestedness_columns <- nested_columns/(ncol(x)*(ncol(x)-1)/2)

  # nestedness of the network

  nestedness_network <- (nested_rows+nested_columns)/((nrow(x)*(nrow(x)-1)/2)+(ncol(x)*(ncol(x)-1)/2))

  return(list(nestedness_rows, nestedness_columns, nestedness_network))

}
