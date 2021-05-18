## This functions gets a matrix and calculates the inverse version of it
## It uses set and get functions to perform the task 

## In the first part calculations are performed

makeCacheMatrix <- function(x = matrix()) {
  ## Initializes the inverse property
  i <- NULL
  
  ## The method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## The method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## The method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## The method to get the inverse of the matrix
  getInverse <- function() {
    ## Returns the inverse property
    i
  }
  
  ## Returns a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## In the second part it will check existence of a cached version to return
## If no cache exists, it returns a new copy.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Returns a set version
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Gets the matrix from our object
  data <- x$get()
  
  ## Calculates the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Sets the inverse to the object
  x$setInverse(m)
  
  ## Returns the matrix
  m
}
