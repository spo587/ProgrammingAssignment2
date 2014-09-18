## The functions below allow you to create a special 'matrix' instance
## and then calculate or retrieve its inverse if it has already
## been set or cached.

## creates a special 'matrix', actually a list containing functions to
## set the value of the matrix, get the value, set the inverse or get the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## a function that either retrieves the already calculated or set value of the 
## inverse of a special matrix created by the function above,
## or if the inverse hasn't been calculated or set, calculates it.

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
