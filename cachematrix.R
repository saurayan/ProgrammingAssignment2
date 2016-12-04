# Start of Instructions
# Please run in the following sequence

# First generate a square matrix of random numbers

# m1<-matrix(runif(16,100,1000),4,4)
# a<-makeCacheMatrix(m1)
# cacheSolve(a)
# cacheSolve(a).... should return the data from cache

# Now try another matrix to check that the value is not fetched from Cache

# m2<-matrix(runif(25,100,1000),5,5)
# b<-makeCacheMatrix(m2)
# cacheSolve(b)
# cacheSolve(b).... should return the data from cache

# End of instrutions

# Starting the body of the function

makeCacheMatrix <- function(x = matrix()) 
  
{
  
  # assigning a value
  i <- NULL
  
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  
  # returning the matrix
  get <- function() x
  
  # setting the inverse value
  setinv <- function(i_inv) i <<- i_inv
  
  # getting the inverse value
  getinv <- function() i
  
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  
  {
        
  # first check if the inverse has already been calculated. 
  # If the value exists in the global environment, then return that value
  i <- x$getinv()
    if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  
  # else, the following section calculates the inverse after checking for 
  
  # first assign the matrix to a variable
  data <- x$get()
  
  # Now first check if the matrix entered is a square and invertible matrix
  # If the matrix is not square and invertible, then prompt for a revised input
  # I am adding these although the assignment asks us to assume that the input is square and invertible
  
  if(isTRUE(nrow(data)!=ncol(data)))
  {
    return("Please change the input to a square matrix")
  }
  
  if(det(data)==0)
  {
    return("You entered a matrix with zero determinant. Please change the input.")
  }
  
  # calculate the inverse
  
  i <- solve(data, ...)
  
  ## Return a matrix that is the inverse of 'x'
  
  x$setinv(i)
  
  i
  
}

# End of the body of the function