## Two functions that allow to cache and retrieve the inverse matrix
## of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Creates a wrapper list for a matrix, that allows to cache 
  # the inversed matrix.
  #
  # Args:
  #   x: An inversible matrix.
  #
  # Returns:
  #   The wrapper list containing functions to set and get matrix data,
  #   and to set and get the cached inversed matrix.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  # Calculates the inversed matrix for x, and caches the result.
  # If the inversed matrix has already been cached, and x hasn't
  # been changed since, returns the cached inversed matrix.
  #
  # Args:
  #   x: A wrapper list, created with the makeCacheMatrix function.
  #   ...: Additional arguments to pass to solve(x, ...).
  #
  # Returns:
  #   The inversed matrix.
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
