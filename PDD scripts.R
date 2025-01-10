
assign_simppay_new <- function(x) {
  vlength <- length(x)
  y=vector(length=vlength)
  for (i in 1:vlength) {
    if (x[i]==1&!is.na(x[i])) {
      y[i] <- 'Medicare'
    } else	if (x[i]==2&!is.na(x[i])) {
      y[i] <- 'Medicaid'
    } else if (x[i]==3&!is.na(x[i])) {
      y[i] <- 'Private'
    } else if (x[i]==8&!is.na(x[i])) {
      y[i] <- 'Uninsured'
    } else if (x[i] %in% c(4,5,6,7,9,10,11)&!is.na(x[i])) {
      y[i] <- 'Other'
    }
  }
  return(y)
}


assign_simpsource_new <- function(x) {
  vlength <- length(x)
  y=vector(length=vlength)
  for (i in 1:vlength) {
    if (x[i] %in% c(1,2,3,5,6,8,9,10,11)&!is.na(x[i])) {
      y[i] <- 'Home'
    } else	if (x[i]==4&!is.na(x[i])) {
      y[i] <- 'Transfer'
    } else if (x[i]==7&!is.na(x[i])) {
      y[i] <- 'ED'
    } else y[i] <- 'Other'
  }
  return(y)
}
