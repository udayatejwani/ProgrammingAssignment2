###########################################################################
## makeCacheMatrix and cacheSolve functions
##
## Author: Udaya K Tejwani
## Date: 10/28/2018
###########################################################################

###########################################################################
## FUNCTION makeCacheMatrix
## This function accepts a matrix object as parameter and caches its 
## inverse.  This function has the following nested functions:
## set(x): To set the matrix object variable to matrix passed as 
##         parameter
## get(): To return the matrix object to be inverted
## setInverse: To invert the matrix object that has been passed as 
##             parameter
## getInverse(): To return the inverted matrix object
##########################################################################

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#########################################################################
## FUNCTION cacheSolve 
## This function inverts a matrix object and returns it.  If the inverse 
## of the matrix object is cached, then the cached matrix object is 
## returned.
#########################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinverse()
  if (!is.null(invmat)) {
    message("getting cached matrix object")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  invmat
}


