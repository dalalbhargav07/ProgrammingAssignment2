
## Matrix inversion is a very costly computation so there is a benefit to cache the inverse of a matrix rather than computing it repeatedly 
## Functions which is below implements the caching of inverse matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse1 <- NULL
    set <- function(y) {
      x <<- y
      inverse1 <<- NULL
    }
    get <- function() x
    setSolve <- function(inv) inverse1 <<- inv
    getSolve <- function() inverse1
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (P.S. The matrix does not changed), 
## then returns inverse from the cache

cacheSolve <- function(x, ...) {
    inverse1 <- x$getSolve()
    if(!is.null(inverse1)) {
      message("getting cached data")
      return(inverse1)
    }
    data <- x$get()
    inverse1 <- solve(data)
    x$setSolve(inverse1)
    inverse1
}
