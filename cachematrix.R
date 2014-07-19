## makeCacheMatrix represents a caching object for matrices
## cacheSolve is a function that utilizes the matrix cache to extract
##   already inverted matrices

## construct a list of functions representing an object (cache) with methods
## for setting/getting

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve solves matrices using solve(), before a matrix is solved
## the matrix cache is checked to see if the same matrix has previously
## been solved and returns that instead. Otherwise the matrix is solved
## and the result is stored in the cache for later retrieval.

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
