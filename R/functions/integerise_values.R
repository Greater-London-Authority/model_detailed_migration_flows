#integerisation function
integerise_values <- function(x) {

  # For generalisation purpose, x becomes a vector
  xv <- as.vector(x) # allows trs to work on matrices

  xint <- floor(xv) # integer part of the weight
  r <- xv - xint # decimal part of the weight
  def <- round(sum(r)) # the deficit population

  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1

  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)

  return(xint)
}

