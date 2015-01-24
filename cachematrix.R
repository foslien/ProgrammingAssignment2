## the following functions are designed to store a cached version of a matrix inverse.
## the intent is to minimize computation. users first call makeCacheMatrix with a standard
## matrix (the matrix must be square). then, to get the inverse of the special cached users
## call "cacheSolve"

## makeCacheMatrix creates a special matrix object. this special matrix can store it's own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(x){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve will compute the inverse of a special "matrix", where that "matrix" was created
## by makeCacheMatrix. if the inverse has already been calculated, we will return the inverse
## that has been stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached invervse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
