##      This overall pair of functions is used to either:
##      compute the inverse of a matrix if no value is 
##      stored in the "cache,: or to return the cached value
##      if it has previously been computed.

##      The makeCacheMatrix function creates a list that 
##      will preserve the variable "m" within
##      its own environment, creating a kind of cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##      This function returns "m" if it has already been 
##      computed for the matrix encoded into a list created
##      by makeCacheMatrix, or else it uses "solve" to compute
##      the inverse and then updates the list accordingly.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
