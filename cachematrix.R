## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
  * set the value of the matrix
  * get the value of the matrix
  * set the value of the inverse matrix
  * get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_inv_matrix <- function(inverse) inv_matrix <<- inverse
  get_inv_matrix <- function() inv_matrix
  list(set = set, get = get,
       set_inv_matrix = set_inv_matrix,
       get_inv_matrix = get_inv_matrix)
}


## The following function solve the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the set_inv_matrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_inv_matrix()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$set_inv_matrix(inv_matrix, ...)
  inv_matrix
}
