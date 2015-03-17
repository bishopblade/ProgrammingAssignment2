## makeCacheMatrix and cacheSolve are two functions designed to calculate the inverse of a matrix,
## a time-consuming operation. makeCacheMatrix will produce a list with four functions used to modify
## a matrix and its inverse, and cacheSolve uses such a list to calculate the inverse. Both
## functions utilize a cache to reduce further calculations once the inverse has already been
## calculated.

## makeCacheMatrix creates a list containing four functions to set the value of its matrix,
## get the value of its matrix, set the value of its inverse, and get the value of its inverse.
## makeCacheMatrix(X) will initialize the list with the value of its matrix set to X.

makeCacheMatrix <- function(mData = matrix()) {
  inverseCache <- NULL                                         ## Initialize cached inverse to null
  
  set <- function(matrix) {                                    ## Sets value of matrix
    mData <<- matrix
    inverseCache <<- NULL
  }
  
  get <- function() mData                                      ## Gets value of matrix
  
  setInvCache <- function(inverse) inverseCache <<- inverse    ## Sets cached inverse 
  
  getInvCache <- function() inverseCache                       ## Gets cached inverse
  
  list(set = set, get = get,                                   ## Return list containing all four functions
       setInvCache = setInvCache, getInvCache = getInvCache)
}


## cacheSolve works in tandem with the list produced by makeCacheMatrix. It first checks the cache to see
## if the inverse has already been calculated, and if so returns it; otherwise it calculates the
## inverse and caches it so that it will be retrieved when the function is next called.
## Any arguments after the first will be passed to the solve() function when the inverse is calculated.

cacheSolve <- function(inverseList, ...) {
  inverseCache <- inverseList$getInvCache()           ## Retrieve cached inverse (if any)
  
  if(!is.null(inverseCache)) {                        ## Check if retrieved inverse has a value
    message("Retrieved cached inverse")
    return(inverseCache)
  }
  
  data <- inverseList$get()                           ## If retrieved inverse was null retrieve the matrix value
  
  inverseCache <- solve(data, ...)                    ## Calculate the inverse of the matrix and cache it
  
  inverseList$setInvCache(inverseCache)
  
  return(inverseCache)
}
