## makeCacheMatrix and cacheSolve serve as functions to compute
## the inverse of a matrix, which can be stored along with the
## matrix itselfs.

## makeCacheMatrix creates a "matrix" that can cache its inverse.
## This "matrix" basically consists of functions to get/set the
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned by
## the function makeCacheMatrix. If the inverse has already been
## calculated, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
