## Function for "matrix" object.
makeCacheMatrix <- function(x = matrix()) {
  # Firstly we need to initialize a variable to store the cached inverse
  inv <- NULL
  
  # And then we need to define a function 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Define a function to get the matrix
  get <- function() x
  
  # Define a function to get the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Return a list containing the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function itself computes the inverse of the special "matrix" by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Check one more time
  inverse <- x$getinverse()
  
  # Just in case that the cached inverse is not available, execute it again with this
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  # If the cached inverse is not available, use this
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Cache the computed inverse
  x$setinverse(inverse)
  
  # Return the computed inverse
  inverse
}
