## makeCacheMatrix creates a list with functions get, set, getsolve, setsolve
## Please run it first to initialize the matrix
## Input could be a random matrix. E.g. 
## > m<-makeCacheMatrix(matrix(rnorm(100),nrow=10,ncol=10))

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
}


## This function tries to invert (solve) the matrix.
## It uses martix/list created with makeCacheMatrix function
## In order to test you could try running the following command:
## > cacheSolve(m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if (!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
