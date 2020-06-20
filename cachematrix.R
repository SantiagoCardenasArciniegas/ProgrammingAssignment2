 ## The purpose of the functions is to make reduce the time and computing capacity spent in
## inverting matrices. 

##This first function creates a Matrix that contains the information that can be passed to
## create its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This second function uses the information stored in the matrix created before, to create
## the inverse. Given that the information for the inverse already exists and that the matrix
## has not changed, the function takes it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
