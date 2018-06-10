## Matrix inversion can be a time-consuming computation.  It could be beneficial to 
## to cache the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix function does the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setinverted <- function(inverse) inverted <<- inverse
  getinverted <- function() inverted
  list(set=set, get=get, setinverted=setinverted, getinverted=getinverted)
}


# The below function returns the inverse of the matrix. First, it checks if
# the inverse has already been computed. If so, it gets the result without the
# computation. If the inverse doesn't exist, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(x, ...) {
  inverted <- x$getinverted()
  if(!is.null(inverted)) {
    message("Pulling data.")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data)
  x$setinverted(inverted)
  inverted
}
