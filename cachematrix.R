## makeCacheMatrix and cacheSolve allow for the result of a matrix
## inversion to be stored in a cache, so that subsequently calling
## up the specialized "cacheSolve" function doesn't need to compute
## an inversion again.

## makeCacheMatrix creates a list containing functions to set and get
## values for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrixinverse <<- inverse
        getInverse <- function() matrixinverse
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## cacheSolve will compute the inverse of your matrix if it has not
## already done so.  If it has, then it will simply pull the value
## out of the list in makeCacheMatrix and print it, after letting you
## know that it was previously cached with "getting cached data"

cacheSolve <- function(x, ...) {
        matrixinverse <- x$getInverse()
        if (!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data, ...)
        x$setInverse(matrixinverse)
        matrixinverse
}