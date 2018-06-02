## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly



## The first function creates a special "vector", which is really a list containing a function to

#* set the value of the matrix
#* get the value of the matrix
#* set the inverse matrix
#* get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The following function calculates the inverse of the special "matrix" 
## created with makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets it from the cache and skips the computation. 
## Otherwise, it calculates it and sets it in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
