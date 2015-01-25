## Project: Coursera programming assignment 2
## Description: 2 functions
##  1. makeCacheMatrix: Accepts a matrix as input and makes it accessible to other functions.
##  2. cacheSolve: accepts the object returned by makeCacheMatrix, checks to determine if the inverted
##                 matrix has already been cached..if true returns it, else inverts, caches it and then retruns inverted matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(mat) m <<- mat
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## First try to pull the cached inverted matrix
        m <- x$getmat()
        #if it is not null then access the cached matrix else the matrix has "changed" and has not been inverted yet
        # my assumption is if makeCacheMatrix has been called then the matrix has changed
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        #matrix has yet to be cached so get the data
        data <- x$get()
        #invert the matrix
        m <- solve(data)
        # call setmat to cache the inverted matrix
        x$setmat(m)
        m
        
}



