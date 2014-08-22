## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.

## makeCacheMatrix creates a special matrix object
## which is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize inverse of the matrix as NULL
  inv <- NULL
  
  # "set" function to store the matrix data to CacheMatrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }#set end
  
  # "get" function to get the matrix data from CacheMatrix
  get <- function() x
  
  # set inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get inverse of the matrix
  getinverse <- function() inv
  
  # list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## makeCacheMatrix function End

## cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function.
## If inverse is available, cacheSolve retrieves it,
## else, it computes, caches, and returns it.
## assumption : the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  
  # get inverse of the CacheMatrix
  inv <- x$getinv()
  
  # check if inverse is already computed for given CacheMatrix
  # if "true" return the  matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # else, get the matrix data from CacheMatrix
  data <- x$get()

  # and compute inverse of it
  inv <- solve(data, ...)

  # store inverse of the matrix
  x$setinv(inv)

  # Return the matrix that is the inverse of 'x'
  inv 
}
## CacheSolve function end
