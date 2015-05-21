## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(matr = matrix()) {
  inver <- NULL
  set <- function(x) {
    matr <<- x;
    inver <<- NULL;
  }
  get <- function() return(matr);
  setinv <- function(inv) inver <<- inv;
  getinv <- function() return(inver);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(matr, ...) {
  inv <- matr$getinv()
  if(!is.null(inver)) {
    message("Getting cached data...")
    return(inver)
  }
  data <- matr$get()
  invserse <- solve(data, ...)
  matr$setinv(inver)
  return(inver)
}