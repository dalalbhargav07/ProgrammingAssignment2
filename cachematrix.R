
## Matrix inversion is a very costly computation so there is a benefit to cache the inverse of a matrix rather than computing it repeatedly 
## Functions which is below implements the caching of inverse matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setSolve <- function(inv) inverse <<- inv
    getSolve <- function() inverse
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (P.S. The matrix does not changed), 
## then returns inverse from the cache

cacheSolve <- function(x, ...) {
    inverse <- x$getSolve()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setSolve(inverse)
    inverse
}
