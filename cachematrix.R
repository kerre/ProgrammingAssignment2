## This pair of functions cache the inverse of a matrix in order to avoid unneccessary 
## computation

## The first function, mackeCacheMatrix, sets up a matrix and a list containg functions to set 
## the matix, get the matrix, set the inverse and get the inverse.

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
             setinverse= setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, first checks if the inverse of the matrix passed 
## has already been calculated. If it has,it gets it from the cache. If not it calculates it 
## using the solve function and then caches that value.  The inverse of the matrix is returned.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
