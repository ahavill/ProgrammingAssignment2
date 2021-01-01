# This program caches the inverse of matrix in order to reduce time-consuming 
# computations.   

# The function makeCacheMatrix creates a special "matrix" object that can 
# cache its inverse.
# The special "matrix" is a list containing a function to: 
# set the value of the matrix, get the value of the matrix, set the value 
# of the inverse, and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- mean
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The function cacheSolve computes the inverse the matrix returned by makeCacheMatrix.
# If the inverse has already been stored, then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

