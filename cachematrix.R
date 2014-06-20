## These two functions will cache the inverse of a matrix and return the cached inverse 
## rather than calculating the inverse each time.

## The makeCacheMatrix function creates a special "vector", which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setinverse function.

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
