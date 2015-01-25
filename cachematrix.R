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
    ## get cached matrix
    get <- function() x
    ## set inverse matrix to cache
    setinv <- function(inv) inverse <<- inv
    ## get cached inverse matrix
    getinv <- function() inverse
    ## return a list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## get cached inverse matrix
    inverse <- x$getinv()
    ## check if the cached value is null
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- x$get()
    ## calculate inverse matrix
    invserse <- solve(data, ...)
    x$setinv(inverse)
    ## return a matrix that is the inverse of x
    inverse
}
