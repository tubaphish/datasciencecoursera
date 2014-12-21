## Functions performing tasks described in Programming Assignment # 2 of 'R Programming' course
## Delivered through John Hopkins University via Coursera
##
## Submitted by: Tyler D. Rudolph
## Dec. 16, 2014

## makeCacheMatrix() calculates inverse of a matrix 'x' and stores it in the function's defining 
## environment

makeCacheMatrix <- function(x = matrix()) {
 
      inv.m <- NULL      
      set <- function(y) {
            x <<- y
            inv.m <<- NULL
      }      
      get <- function() x
      setsolve <- function(solve) inv.m <<- solve
      getsolve <- function() inv.m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
      
}      


## cacheSolve() returns a matrix that is the inverse of 'x', first seeking value within cache

cacheSolve <- function(x, ...) {
      
      # Retrieve inverse matrix from cache and return if not empty (null)
      inv.m <- x$getsolve()
      if(!is.null(inv.m)) {
            message("getting cached data")
            return(inv.m)
      }
      
      # Calculate inverse matrix and return
      data <- x$get()
      inv.m <- solve(data, ...)
      x$setsolve(inv.m)
      inv.m
      
}
