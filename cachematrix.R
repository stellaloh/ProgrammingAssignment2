## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## Here is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse
## 4. get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- NULL
  set <- function(y) {
    x <<- y
    inverse_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve_mat) inverse_mat <<- solve_mat
  getinverse <- function() inverse_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse_mat <- x$getinverse()
  if(!is.null(inverse_mat)) {
    message("getting cached data")
    return(inverse_mat)
  }
  data <- x$get()
  inverse_mat <- solve(data, ...)
  x$setinverse(inverse_mat)
  inverse_mat
}
