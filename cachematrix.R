## functions for calculating and caching the inverse of a matrix
## functions assume the matrix is invertible and do no error checking to insure this

## makeCacheMatrix returns a list of functions for setting and getting an invertible matrix
## as well as setting and getting it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve returns the inverse of an invertible matrix 
## if the inverse has already been computed, the cached version is returned
## otherwise the function uses the built in "solve" function to calculate the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
