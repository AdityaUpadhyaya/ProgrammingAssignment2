

## Function makeCacheMatrix takes a matrix and creates a special matrix
## that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  seti <- function(solve) I <<- solve
  geti <- function() I
  list(set = set, get = get,
       seti = seti,
       geti = geti) ## list of above defined 4 functions.

}


## Function cacheSolve below computes the inverse of special matrix created
## using above makeCachematrix function. If the inverse has already been 
## calculated then inverse from cache is retrived.

cacheSolve <- function(x, ...) {
  I <- x$geti()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$seti(I)  ## save inverse to cache.
  I
}
