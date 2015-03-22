## List below are functions used to to cache potentially time-consuming 'inverse matrix' computations
## Invariably, the contents of a matrix does not change. In such scenarios it makes sense to cache 
##    the value of the 'inverse matrix' so that when we need it again, 
##    it can be looked up in the cache rather than recomputed. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	iM <- NULL
        set <- function(y) {
                x <<- y
                iM <<- NULL
        }
        get <- function() x
        setInv <- function(solve) iM <<- solve
        getInv <- function() iM
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##    method above. If the inverse has already been calculated (and the matrix has not changed),
##    then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        iM <- x$getInv()
        if(!is.null(iM)) {
                message("getting cached data")
                return(iM)
        }
        data <- x$get()
        iM <- solve(data, ...)
        x$setInv(iM)
        iM
}
