## makeCacheMatrix and cacheSolve follow the design shown in the example functions for the assignment.
## They produce and work with a list, respectively, that is able to calculate the inverse of a matrix.

## makeCacheMatrix creates a list containing four functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) i <<- inverse
  
  getinv <- function() i
  
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## cacheSolve works in tandem with the list produced by makeCacheMatrix. It first checks to see
## if the inverse has already been calculated, and if so returns it; otherwise it calculates the
## inverse and caches it so that it will be retrieved when the function is next called.

cacheSolve <- function(x, ...) {
  i <- x$getinv() ## Retrieve cached inverse (if any)
  
  if(!is.null(i)) {
    message("Retrieving cached inverse")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data)
  
  x$setinv(i)
  
  i
}
