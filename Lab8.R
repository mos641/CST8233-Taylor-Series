# Lab 8
# Taylor Series

plotGraph <- function(numX){
  # loop to calculate ln(x) for each of the 100 terms
  i <- 1
  # vectors to store data
  currentVal <- 0
  lnx <- vector(mode = "numeric", length = 100)
  while (i <= 100){
    # calculate the term and store result in vector
    currentVal <- currentVal + ( ( ((-1)^(i+1)) * factorial(i-1) ) / ( (1) ^ i ) / factorial(i) ) * ( (numX - 1) ^ i )
    lnx[i] <- currentVal
    
    # increment i
    i <- i + 1
  }
  
  plot(lnx)
}

# function for taylor series of ln(x) around 1, n = 10
taylorSeries <- function(){
  # create a matrix for value storage
  taylorMatrix <- matrix(1:40, nrow = 10, ncol = 4)
  colnames(taylorMatrix) <- c("Term", "ln(x)", "  Absolute Error", "  Relative Error")
  
  # ask for input
  numX <- readline(prompt = "Please enter the value of x: ")
  numX <- as.integer(numX)
  
  # loop to calculate ln(x) for each of the ten terms, and errors
  i <- 1
  # vectors to store data
  currentVal <- 0
  derivativesVec <- vector(mode = "numeric", length = 10)
  lnx <- vector(mode = "numeric", length = 10)
  while (i <= 10){
    # calculate the term and store result in vector
    derivativesVec[i] <- ( ((-1)^(i+1)) * factorial(i-1) ) / ( (1) ^ i )
    lnx[i] <- ( derivativesVec[i] / factorial(i) ) * ( (numX - 1) ^ i )
    currentVal <- currentVal + lnx[i]
    taylorMatrix[i,2] <- currentVal
    
    # calculate the absolute error and store
    taylorMatrix[i,3] <- taylorMatrix[i,2] - log(numX, base = exp(1))
    
    # make sure it is positive
    if (taylorMatrix[i,3] < 0){
      taylorMatrix[i,3] <- taylorMatrix[i,3] * -1
    }
    
    # calculate the relative error and store
    taylorMatrix[i,4] <- (taylorMatrix[i,3] / log(numX, base = exp(1))) * 100
    
    # increment i
    i <- i + 1
  }
  
  # print the table
  prmatrix(taylorMatrix, rowlab=rep("",10))
  
  plotGraph(numX)
  
  #print(numX)
  #print(derivativesVec)
  #print(lnx)
}


# call function
taylorSeries()
