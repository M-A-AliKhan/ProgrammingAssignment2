## Creating a matrix object by makeCacheMatric, and lead cacheSolve to compute inverse of matrix

## it first checks to see If the matrix inverse has already been calculated, if so it get it from the cache and skips the computation
## Otherwise, it calculates the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
get <- function() x
setreverse <- function(reverse) m <<- reverse
getreverse <- function() m
list(set = set, get = get,
     setreverse = setreverse,
     getreverse = getreverse)
  
}

## Following function calculates the Matrix Inverse created with above function. However, if it has been calculated, it gets it from cache and skips. otherwise it will calculate again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getreverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- reverse(data, ...)
  x$setreverse(m)
  m
}

