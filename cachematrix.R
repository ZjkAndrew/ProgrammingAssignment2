# Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly (there 
# are also alternatives to matrix inversion that we will not discuss here). 
# This file contains a pair of functions that cache the inverse of 
# a matrix.
# Assuming the input matrix is always invertible.

# ##############################################################################

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

makeCachexrix <- function(x = xrix()) {
	inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) inverse <- inv
    
    getinv <- function() inverse
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# ##############################################################################

## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrux that is the inverse of 'x'
	inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
