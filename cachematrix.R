## Cache the Inverse of an invertible Matrix in order to avoid repeated 
## computation using a pair of functions
## As a precondition, the matrix should be invertible, else an error is raised
##
## Usage example with an invertible matrix:
## mm = matrix(c(4,3,3,2),2) # create matrix
## mcm <- makeCacheMatrix(mm) # convert into our special "matrix"
## cacheSolve(mcm) # calculate the inverse and store it for the next call

## Create an object which allows to store a matrix and its inverse using getters
## and setters

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL  # initialize object for inversed matrix
    
    set <- function(y) {  # set a matrix, reset the inverse matrix
        x <<- y  # change x in parent environment (the makeCacheMatrix parameter!)
        i <<- NULL  # initialize (reset) inversed matrix in parent environment
    }
    
    get <- function() x  # return the matrix
    
    setinverse <- function(solved) i <<- solved  # assign the given inverse to i object in parent environment
    
    getinverse <- function() i  # return inversed matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  # return our special "matrix", wrapped as a list
}


## using the function makeCacheMatrix, calculate the inverse of a given matrix
## either by actually performing the calculation or retrieve the cached inverse

cacheSolve <- function(x, ...) {
    
    ## check whether inverse was already calculated (cached)
    i <- x$getinverse()
    if(!is.null(i)) {  
        message("getting cached data")
        return(i)  # return the cached inversed matrix
    }
    
    ## as the inverse has not yet been calculated (cached), perform caculation
    data <- x$get()
    i <- solve(data, ...)  # calculate the inversed matrix
    x$setinverse(i)  # cache the inversed matrix 
    
    i  # return the calculated inversed matrix
}
