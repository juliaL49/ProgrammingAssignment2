## To avoid the costly computation of the inverse of a matrix where possible, 
## we are first check whether it is already cached.
## If so, we return the cached matrix, otherwise compute and return.
## Assumption: supplied matrix is always invertible.

## Create a special matrix object that can cache its inverse.
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


## Compute the inverse of the above special matrix. Return inverse from cache 
## if it has already been calculated (and the matrix has not changed).
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
