## makeCacheMatrix creates a function that stores a matrix in the cache
## cacheSolve generates the inverse of the matrix

## creates a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    getinverse <- function(solve) i <<- solve
    getinverse <- function () i
    list(set = set, get = get,
    setinverse = setinverse, getinverse = getinverse)
    
}


## returns the inverse of matrix x

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
