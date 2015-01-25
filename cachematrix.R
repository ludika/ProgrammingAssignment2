## The following functions let compute and store the inverse of a
## matrix; in this way it's possible to avoid repeating the same
## calculation.

## This function creates a matrix object that can cache the matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invx <<- solve
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function returns the value of the inverse of the matrix if
## it's already present in 'makeCacheMatrix' above, otherwise it 
## computes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}