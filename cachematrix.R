## These functions will invert a matrix and cache its value so it doesn't
## need to recalculate values that already exist.

## makeCacheMatrix creates a list of functions to cache a matrix and it's inverse:

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL                       
  
  set <- function(y) {            ## Create a function that caches a matrix and a vector
    x <<- y                       ## with a NULL value indicating that it hasn't been inverted
    m <<- NULL                    
  }
  get <- function() x             ## create a function to return the cached matrix
  setinverse <- function(inverse) m <<- inverse
                                  ## Create a function to cache a matrix that has been passed to it
  getinverse <- function() m      ## Create a function to return the cached inverted matrix
  list(set = set,get = get,       ## Return the list of functions as output from the function call
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Determines if a matrix has already been inverted.Inverts the matrix and caches it if it hasn't been inverted.
## Returns the previously cached inverted matrix if it has been inverted before.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()             ## Return a matrix that is the inverse of 'x' or NULL if
                                  ## the matrix hasn't been inverted
  if(!is.null(m)) {               ## If it isn't NULL then return the cached value
    message("getting cached data")
    return(m)
  }
  data <- x$get()                 ## if it is NULL, use the get function to pull the base matrix,
  m <- solve(data, ...)           ## solve for the inverse
  x$setinverse(m)                 ## and use the setinverse function to cache the inverted matrix
  m                               
  
}
