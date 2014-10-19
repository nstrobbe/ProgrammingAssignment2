## In this file there are two functions that will allow to compute the inverse 
## of a matrix, and cache its results. 
## In this way we will not have to recompute the inverse every time we want to 
## use it. This can be a great speed increase for large matrices. 
##############################################################################

## The makeCacheMatrix function takes a matrix as argument. 
## It will allow to set/get the value of the matrix, and set/get its inverse. 
## In addition it supports the use of caching to speed up computations.
## The function returns a list containing four functions: 
##     set           Set the elements of the matrix to a new value
##     get           Return the matrix
##     setinverse    Manually set the inverse of the matrix
##     getinverse    Return the stored value for inverse of the matrix
## The inverse is stored in the environment associated with the cached matrix,
## upon creation of the CacheMatrix it is initialized to NULL. 

makeCacheMatrix <- function(x = matrix()) {
        # Print a warning if the matrix is not square
        if (nrow(x) != ncol(x)){
                warning("Input is not a square matrix! ",
                        "Any attempt to compute the inverse will result in error. ")
        }
        
        # create variable "inverse" to be stored in the environment
        inverse <- NULL
        
        # create function to change the values of the matrix and put 
        # inverse back to NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # create function that returns the matrix
        get <- function() {
                x
        }
        
        # create function that manually sets the inverse of the matrix
        setinverse <- function(inv) {
                inverse <<- inv
        }
        
        # create function that returns the stored value for inverse
        getinverse <- function() {
                inverse
        }
        
        # return a list containing the above defined functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will return the inverse of a matrix.
## The matrix needs to be converted into a "cached matrix" by the method above.
## If the inverse has not been calculated before, this function will do that
## and cache the result. If cacheSolve is called again after that, it will 
## return the value it finds in the cache.

cacheSolve <- function(x, ...) {
        # Get the current value for the inverse
        inverse <- x$getinverse()
        
        # If there was something stored, just return that value
        # No need to recompute
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # The inverse was not stored yet, so we need to compute it
        # First get the actual matrix
        data <- x$get()
        
        # Then check that the matrix is square
        # If not, print out message and exit
        if (nrow(data) != ncol(data)) {
                stop("Impossible to invert a non-square matrix!!! ", 
                     "Don't say I didn't warn you...")
        }
        
        # Compute the inverse using the solve function
        inverse <- solve(data, ...)

        # Set the inverse (in the cache)
        x$setinverse(inverse)
        
        # Return the inverse
        inverse
}
