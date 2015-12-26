## This program contains a pair of functions that cache the inverse of a matrix. 
## makeCacheMatrix is the function that creates a special "matrix" object and caches its inverse.
## cacheSolve computes that inverse of the special "matrix" returned by makeCacheMatrix. If the inverse
## has already been calculated, then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix creates a special "matrix" object, which contains a list of functions:
## set(): set the value of the matrix
## get(): get the value of the matrix
## setInv(): set the value of the inverse
## getInv(): get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(mean) m <<- mean
  getInv <- function() m
  list(set=set, get=get,
       setInv=setInv,
       getInv=getInv)

}

## cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

