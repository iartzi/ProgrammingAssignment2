# The first function, makeCacheMatrix creates a matrix, 
# which is a list containing a function to:

# Set the value of the matrix
# Get the value of the matrix
# Set the value of the inverse matrix
# Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inverseX <- NULL
      set <- function(y) {
                 x <<- y
          inverseX <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inverseX <<- inverse
      getInverse <- function() inverseX
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

# The following function calculates the inverse of the matrix created with the above 
# function. It first checks to see if the inverse matrix has already been calculated. 
# If the inverse matrix has been calculated, it gets the inverse matrix from the cache 
# and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the matrix
# in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverseX <- x$getInverse()
      if (!is.null(inverseX)) {
            message("Getting cached inverse matrix")
            return(inverseX)
      } else {
            inverseX <- solve(x$get())
            x$setInverse(inverseX)
            inverseX
      }
}
