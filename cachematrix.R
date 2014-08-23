## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
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


## Write a short comment describing this function

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

test_vnorguet_code <- function(){
  testMatrix <- matrix(
    c(2, 4, 3, 1, 5, 7, 2, 5, 1),
    nrow=3,
    ncol=3)
  demo <- makeCacheMatrix(testMatrix)
  ## Not cached here
  cacheSolve(demo)
  # Should be cached
  cacheSolve(demo)
}