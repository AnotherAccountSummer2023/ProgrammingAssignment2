##     General description
## makeCacheMatrix() - return a "special" matrix which preserve a given
## matrix() as well as its inverse.
## cacheSolve() - responsible for access to an inverse matrix from the previous
## "special" object. Return the cached inverse or calculate it, save, and 
## then return.

## This function creates a special "matrix" object that can cache its inverse
## Simply - just a struct with set/get methods only
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse_matrix) inverse <<- inverse_matrix
  get_inverse <- function() inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  temp <- x$get_inverse()
  if (!is.null(temp)){
    message("Used the cached matrix.")
    return(temp)
  }
  x$set_inverse(solve(x$get()))
  x$get_inverse()
}
