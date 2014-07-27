## Function are used for cached execution of matrix inverse
## function will return cached value if already calculated

## This function defines getters and setters for value of matrix and solve function

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(solve) invm <<- solve
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
  
}


## cacheSolve returns inverse of matrix from cache if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvm(m)
  m
}
