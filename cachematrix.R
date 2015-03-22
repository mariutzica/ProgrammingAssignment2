## The set of functions in this file cache computed values for the inverse of a matrix. The matrix must be 
## first initialized with makeCacheMatrix(). The function cacheSolve() can then be used on this special matrix
## to either pull up its inverse from the cache or, if it hasn't already been computed, compute the inverse and 
## then store it in the cache.

## This function creates a special "matrix", which is really a list containing functions to
## set(x) - set the value of the matrix to x
## get() - grab the value of the matrix
## setinverse(i) - set (not compute) the value of the inverse to i
## getinverse() - grab the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse to NULL (not yet computed)
  i <- NULL
  ## Initialize the four functions that can be used to access/assign values of the matrix and its inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.
## This function is based on the format of the function cachemean provided in the Programming Assignment 2 
## description. It assumes that inputted matrices have an inverse.
cacheSolve <- function(x, ...) {
  ## Check to see if an inverse has already been calculated. If so, grab from cache and return.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Compute and return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
