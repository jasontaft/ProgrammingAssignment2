## a pair of functions that computes and caches the inverse of a matrix.


## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  theInverse <- NULL
  set <- function(y) {
    x <<- y
    theInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(anInverse) theInverse <<- anInverse
  getInverse <- function() theInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  theInverse <- x$getInverse()
  if(!is.null(theInverse)) {
    message("getting cached matrix inversion")
    return(theInverse)
  }
  data <- x$get()
  theInverse <- solve(data, ...)
  x$setInverse(theInverse)
  theInverse
}
