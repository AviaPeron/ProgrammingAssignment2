## Put comments here that give an overall description of what your
## functions do the calculations of matrix invertion and cache the inverse of a matrix.

## This function creat a special "matrix" object that can cache its inverse and define the conduct of the process.

makematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(long) inv <<- long
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## In this function the calculation is happening. notice that in 'makematrix' you can't find the 'solve' function. If the calculation has already happend, the cachesolve should retrieve the inverse from the cache.

cachsolve <- function(x, ...) {
  inv2 <- x$getinv()
  if (!is.null(inv2)) {
    message("getting cached data")
    return(inv2)
  }
  stat <- x$get()
  inv <- solve(stat, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
