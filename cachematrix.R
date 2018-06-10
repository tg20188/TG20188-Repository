## This function is trying to cache the inverse of a matrix rather than compute it. 
## The first functions set the value of a matrix and then gets this value. Following by
## calculating and getting the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inverted_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverted_matrix <<- NULL
  }
  get <- function() x
  setinverted <- function(inverse) inverted_matrix <<- inverse
  getinverted <- function() inverted_matrix
  list(set=set, get=get, setinverted=setinverted, getinverted=getinverted)
}


# The below function returns the inverse of the matrix. If the inverse has 
## already been calculated it gets the result. 
## If the inverse doesn't exist, it computes the inverse.


cacheSolve <- function(x, ...) {
  inverted_matrix <- x$getinverted()
  if(!is.null(inverted_matrix)) {
    message("Be patient!!!")
    return(inverted_matrix)
  }
  data <- x$get()
  inverted_matrix <- solve(data)
  x$setinverted(inverted_matrix)
  inverted_matrix
}
