## Put comments here that give an overall description of what your functions do
# Caching the Inverse of a Matrix

## Write a short comment describing this function
# creates a special "matrix" object that can cache its inverse, which is really a list containing functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # inversed matirx
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


## Write a short comment describing this function
# computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached inversed matrix")
        return(m)
    }
    mtx_data <- x$get()
    m <- solve(mtx_data, ...)
    x$setinverse(m)
    m  ## Return a matrix that is the inverse of 'x'
}

# example
# mtx_origin <- matrix(1:4, 2, 2)
# list_mcm <- makeCacheMatrix(mtx_origin)
# class(list_mcm)
# list_mcm$getinverse()
# cacheSolve(list_mcm)
# list_mcm$getinverse()
