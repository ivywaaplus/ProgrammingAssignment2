## The makeCacheMatrix() function creates a special matrix 
## object that can cache its inverse.This function 
## contains four functions: set the value of the matrix;  
## get the value of the matrix; set the value of the inverse 
## and get the value of the inverse. 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) { 
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse1) inverse <<- inverse1
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve() function calculate the inverse of a matrix. 
## If the inverse has been calculated, it prints "getting cached data"
## and retrieves the cached inverse. If the inverse has not been calculated, 
## it calculates the inverse and returns the value of the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
