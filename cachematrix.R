## The following functions cache the inverse of a matrix.

## This function creates a special "matrix" object 
## that can cache its inverse.
## set - set the value of the matrix
## get - get the value of the matrix
## setinv - set the value of the inverse of the matrix
## getinv - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- x$get()
    invserse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
