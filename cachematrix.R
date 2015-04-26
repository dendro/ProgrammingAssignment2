## Cache the Inverse of an invertible Matrix in order to avoid repeated 
## computation using a pair of functions
## As a precondition, the matrix should be invertible, else an error is raised
## Usage example with an invertible matrix:
## mm = matrix(c(4,3,3,2),2)
## mcm <- makeCacheMatrix(mm)
## cacheSolve(mcm)

## Create an object which allows to store a matrix and its inverse using getters
## and setters

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # initialize object for inversed matrix
    set <- function(y) {  # assign a value to an object in parent environment
        x <<- y  # change x in parent environment (makeCacheMatrix parameter!)
        i <<- NULL  # initialize inversed matrix in parent environment
    }
    get <- function() x  # return the matrix
    setinverse <- function(solve) i <<- solve  # calculate inverse matrix
    getinverse <- function() i  # return inversed matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  # wrap our special "matrix" as a list
}


## using the function makeCacheMatrix, calculate the inverse of a given matrix
## either by actually performing the calculation or retrieve the cached inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {  # check whether inverse was already calculated (cached)
        message("getting cached data")
        return(i)  # the cached inversed matrix
    }
    ## as the inverse has not yet been calculated (cached), perform caculation
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)  # cache the inversed matrix 
    i  # Return a matrix that is the inverse of 'x'
}
