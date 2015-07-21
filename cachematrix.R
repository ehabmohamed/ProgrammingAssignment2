## The objective of the below functions is to cache the inverse of a matrix
## so that when it is needed again, it can be looked up in the cache 
## rather than recomputed.

## This function creates a special "matrix" and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix) m <<- matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" 
## created with makeCacheMatrix function

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
