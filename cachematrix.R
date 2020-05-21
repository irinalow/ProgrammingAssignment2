# Task: Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- NULL
  set <- function(y) {
    x <<- y
    inverse_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse_mat <<- inv
  getinverse <- function() inverse_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_mat <- x$getinverse()
  if(!is.null(inverse_mat)) {
    message("getting cached data")
    return(inverse_mat)
  }
  data <- x$get()
  inverse_mat <- solve(data)
  x$setinverse(inverse_mat)
  inverse_mat
}
