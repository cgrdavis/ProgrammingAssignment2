## Program returns the inverse of an array that was entered
## It caches the value if it was already called and not changed

##Function that accepts matrix and performs specific operations on it
makeCacheMatrix <- function(x=numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Cacheing function depending on whether or not variable is null
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
    ## Checking to see if m is NULL or not - If not, pull from cache
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
