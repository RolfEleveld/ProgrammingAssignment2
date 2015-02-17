## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly (there are also alternatives to matrix inversion that we will 
## not discuss here). The below solution contains a pair of functions that 
## cache the inverse of a matrix
## Computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix, then solve(X) returns
## its inverse

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # setter of data
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # getter of data
    get <- function() x
    # setter of inverse
    setInverse <- function(inverse) m <<- inverse
    # getter of inverse
    getInverse <- function() m
    # returning the encapsulated methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##   This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(is.null(m)) {
        message("seeding cache with data")
        data <- x$get()
        # the actual calculation of inverse
        ## ginv inverse relies on the MASS package!; library(MASS)
        m <- solve(data, ...)
        #updating the associated (cached) inverse
        x$setInverse(m)
    }
    message("getting cached data")
    return(m)
}
## Test with:
#c=rbind(c(1, -1/4), c(-1/4, 1))
#cc<-makeCacheMatrix(c)
#cacheSolve(cc)
#solve(c)
#these should be the same
##cacheSolve(cc) # should not show the message seeding the cache.
#cc<-makeCacheMatrix(c) #resets the value
##cacheSolve(cc) # should  show the message seeding the cache again.