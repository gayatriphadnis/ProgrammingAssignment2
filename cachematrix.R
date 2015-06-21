## These two functions together are used to create a special object  
## that stores an invertible matrix and caches its inverse.

## The function makeCacheMatrix creates a special object that can 
## store a matrix, calculate and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by above function.
## If the inverse has already been calculated for the matrix, then cachesolve retrieves the inverse from the cache.
## If the matrix is changed, then cachesolve calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
