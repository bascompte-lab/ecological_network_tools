# Code for the equifrequent null model

equifrequent_model <- function(d,t_max){

  rows <- nrow(d)

  columns <- ncol(d)

  t <- 1

  null_nest = rep(NA,t_max)

  while (t < t_max + 1) {

    B <- matrix(0, rows, columns)

    number_ones <- nnzero(d)

    count_ones <- 0

    while (count_ones < number_ones) {

      x <- ceiling(runif(1, min = 0, max = rows))

      y <- ceiling(runif(1, min = 0, max = columns))

      while (B[x,y] == 1) {

        x <- ceiling(runif(1, min = 0, max = rows))

        y <- ceiling(runif(1, min = 0, max = columns))

      }

      B[x,y] <- 1

      count_ones <- count_ones + 1;

    }

    # Remove unconected species if present
    B = trim_network(B)
    null_nest[t] = nestedness_s(B)[3]

    t<-t+1

  }

  return(null_nest)

}
