## Sometimes it is too computationally intensive to repeatedly calculate the inverse
## of a large matrix.  These two functions create a data structure to cache the inverse
## of a matrix so to calculate it only once.

## Usage:  result <- makeCacheMatrix(aMatrix)
##        inverse = cacheSolve(result)
##
## Note: It is up to the user to ensure the matrix stored in makeCacheMatrix has not
##       changed between storing it and requesting the inverse.


## makeCacheMatrix creates a data structure to hold the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inver) inv <<- inver
   getinv <- function() inv
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve returns the inverse of a matrix stored in the makeCacheMatrix data structure.
## If the inverse is not present, this function will calculate it and then store it for
## future requests.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)  ## cached inverse
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv)  ## create the inverse only once
   inv
}
