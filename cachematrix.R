## Below we have a set of 2 functions that helps cache the inverse of a matrix
## so that this could be used repeatedly whenever there is a need to use the 
## inverse matrix operation. The expensive system operation could be reused by 
## accessing the inverse matrix on the runtime.

## makeCacheMatrix function takes a Matrix as input, inverse the matrix using 
## the solve function and then cache the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i_m <- NULL
  set <- function(y) {
    x <<- y
    i_m <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) i_m <<- solve
  getinvmat <- function() i_m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}
  

## cacheSolve function first checks if the inverse matrix is available for use.
## Otherwise repeats the solve function on the input matrix.

cacheSolve <- function(x, ...) {
    i_m <- x$getinvmat()
    if(!is.null(i_m)) {
      message("getting cached data")
      return(i_m)
    }
    data <- x$get()
    i_m <- solve(data, ...)
    x$setinvmat(i_m)
    i_m
  ## Any matrix that is provided as an input to the above set of functions, 
  ## the inverse of the same is always returned.
    
}
