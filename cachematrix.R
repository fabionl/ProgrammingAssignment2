## These functions allows to cache the inverse of a matrix.
##   To use these function, you should create a squared matrix, applying it to
##     the makeCacheMatrix function. Then, you should apply the makeCacheMatrix
##     object to the cacheSolve function.
##       Example:
##         source("cachematrix.R")
##         mcm <- makeCacheMatrix(x)
##         mcm$getsolve()   # NULL
##         cacheSolve(mcm)  # does the inversion and returns the inversed matrix.
##         mcm$getsolve()   # shows the inversed matrix.
##         cacheSolve(mcm)  # displays a message the its getting the cached data and returns the inversed matrix.



## Creates a cacheable matrix from a given matrix.
makeCacheMatrix <- function(x = matrix()) {
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


## Work with the cacheable matrix, allowing to use the cache when the matrix
##   has not yet been used.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
