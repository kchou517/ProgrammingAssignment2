# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(get = get, set = set,
       getinverse = getinverse,
       setinverse = setinverse)
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("gettting cached data")
    return(inverse)
  }
  
  matrix_data <- x$get()
  inverse <- solve(matrix_data, ...)
  x$setinverse(inverse)
  return(inverse)
}
