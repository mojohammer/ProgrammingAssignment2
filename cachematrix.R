## This source file contains the functions needed to manage the caching of results after
## computing the inverse of a matrix. 
## 

## makeCacheMatrix provides the getters and setters for the created matrix and its
## computed inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(s) m <<- s
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix. It checks cache for a previously
## computed value. If found, it is returned. If not, the inverse is computed and 
## stored. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
