## Functions that provide the capabilities to cache the inverse of a matrix 
## This capabilities are implemented as a function (makeCacheMatrix())
## that creates setter and getter methods for the matrix and the cached value, 
## and a wrapper function to solve() (cacheSolve()) that makes use of 
## the cached value of the inverted matrix.
##
## Assumptions: The code assumes the matrix we are trying to invert is 
## invertible (i.e., the code does not perform any checks on the supplied
## matrix).
##
## Date: 2015/Jul/25


## Function that takes a matrix as argument and creates an object with that 
## matrix and the cached value for the corresponding inverted matrix.
## The object also provides get and set methods for both the matrix
## and the cached value (NOTE: if the inverse value of the matrix has not
## computed and saved with the setinverse() function, getinverse() will
## return a NULL value)
##
## Parameters:
##    x: A matrix object
## Returns:
##    A list with the functions defined 
makeCacheMatrix <- function (x = matrix ()) {
    # Cached inverse matrix of x
    # By default it is NULL until we set it with setinverse ()
    invmatrix <- NULL
    
    # Set function to update the value of x
    # Updating x means we have to reset the cached value to NULL
    #
    # Parameters:
    #   x: The new matrix to store
    set <- function (y) {
        x <<- y
        invmatrix <<- NULL
    }
    
    # Get function to obtain the value of the stored matrix
    #
    # Returns:
    #   The matrix currently stored in x
    get <- function () {
        x
    }
    
    # Set function to update the value of the cached inverse matrix
    #
    # Parameters:
    #   newinversmatrix: New inverse matrix to store
    setinverse <- function (newinversematrix) {
        invmatrix <<- newinversematrix
    }
    
    # Get function to obtain the cached value of the inverse matrix
    # If the cached value has not been updated since the matrix (x) was
    # created or updated, this method returns NULL.
    #
    # Returns:
    #   The cached inverse matrix
    getinverse <- function (){
        invmatrix
    } 
    
    # List to return. This list has the functions we just defined, and is,
    # for practical purposes, the API of this object
    list (set = set, 
          get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## Function that takes an object created with the function makeCacheMatrix ()
## as argument and returns the inverse of the matrix stored in that object.
## This function checks if the object passed as an argument has the value of
## the inverse already cached. If not, this function computes the inverse 
## matrix, stores it in the object, and returns it. If the cached value is 
## set, this function just returns it.
## 
## This function assumes the matrix we are trying to invert is invertible.
## If it isn't, the solve() function will throw an error when invoked.
##
## Parameters:
##    x:       An object created with the makeCacheMatrix() function
##    verbose: A flag to indicate whether to print some messages to follow 
##             the logic of the function. Default value: FALSE
## Returns:
##    The inverted matrix of the one stored in the argument object x.
cacheSolve <- function(x, verbose = FALSE, ...) {
    # Get the cached value of the inverted matrix. This may be NULL if the 
    # cached value has not been set yet.
    invmatrix <- x$getinverse ()
    if (verbose) {
        message ("Got the cached value of the inverted matrix. ")
    }
    
    # If invmatrix is NULL, it means we have to compute the inverted matrix
    # by calling solve(). 
    if (verbose) {
        message ("Is the cached value NULL?  ", is.null (invmatrix))
    }
    if (is.null (invmatrix)) {
        if (verbose) {
            message ("Cached value not available. ",
                     "Computing now by calling solve().")
        }
        # Call solve on the matrix stored in x ....
        invmatrix <- solve (x$get (), ...)
        # ... and save the obtained value (the inverse) in the cache in x
        x$setinverse (invmatrix)
    }
    # else we already have the value we were looking for.
    
    # Return the inverse matrix.
    invmatrix
}
