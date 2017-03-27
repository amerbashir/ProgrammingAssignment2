## Week3 Assignment - Caching the Inverse of a Matrix


## makeCacheMatrix() 
## It stores and gets a matrix and it's inverse matrix in an another environment.

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
      x <<- y
      invrs <<- NULL
    }
    
    get <- function() {
      x
    }
    
    setInverse <- function(inverse) {
      invrs <<- inverse
    }
    
    getInverse <- function() {
      invrs
    }
    
    list( set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve()
## it retuns an invers of a matrix, it tries to fetch the inverse matrix stored in a cache, but
## if inverse matrix is not in the cache yet, it creates an inverse of the matrix, stores the inverse
## matrix in the cache, and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("Getting cached data")
    return(inver)
  }
  
  message("... Creating cached data")
  mtx <- x$get()
  inver <- solve(mtx, ...)
  x$setInverse(inver)
  inver
}
