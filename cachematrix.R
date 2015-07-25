## These functions create a special 'Matrix' that can cache its inverse
## it will store the inverse of the matrix inside itself and return that
## if it exists so that it doesn't need to recalculate the inverse if it
## is already solved.

## makeCacheMatrix creates a special matrix that can contain its inverse and
## has methods to set and get the matrix and to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve will compute the inverse of a CacheMatrix and will check
## to see if the inverse is already calculated and cached and will use
## that if available to avert recalculating the inverse if it has already
## been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
