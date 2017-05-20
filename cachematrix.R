## This pair of functions both calculates the inverse of a square matrix and
## caches the results, which saves computational resources when ran
## multiple times.  

## The makeCacheMatrix function takes a matrix as an argument and builds a list
## vector which are a set of functions and returns the functions within a list.

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

## The cacheSolve function either calculates or retrieves the inverse of a matrix,
## depending on whether the inverse of a matrix has already been cached.

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