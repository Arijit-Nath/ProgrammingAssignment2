## This program is used to cache the inverse of a Matrix to improve the performance. As we know that Matrix inversion  
## is usually a costly computation.

## It creates a new Matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function creates an inverse of the matrix created by the function makeCacheMatrix. If the inverse is already
## calculated, then it should return the cached inverse matrix, otherwise compute the 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    message("calculating inverse")
    inv
}

