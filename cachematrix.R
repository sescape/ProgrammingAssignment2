## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  
  ## get and set functions
  set <- function(matrix) {
    x <<- matrix;
    
    ##set inverse to null 
    mInverse <<- NULL;
  }
  get <- function() {
    return(x)
  }
  
  setInverse <- function(inverse) {
    mInverse <<- inverse;
  }
  
  getInverse <- function() {
    return(mInverse);
  }
  
  spMatrix <- list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
  return(spMatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, then this method retrieves the inverse from the cache instead of recalculating
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getInverse()
  
  ## if matrix inverse in not null then get the cached data
  if(!is.null(matrixInverse)) {
    message("Retrieving inverse from the cache")
    return(matrixInverse)
  }
  
  ## if matrix inverse in null, calculate it and cahce it
  matrixData <- x$get()
  matrixInverse <- solve(matrixData, ...)
  
  ## cache newly created inverse
  x$setInverse(matrixInverse)
  
  message("Calculated inverse")
  return(matrixInverse)
}
