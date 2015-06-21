## This pair of functions calculates and caches the inverse
## of an invertible square matrix

## creates a list of four functions that get/set a matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(A) {
    x <<- A
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given a matrix x, this function either calculates and caches the inverse
## or retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...=...)
  x$setinverse(i)
  i
}
