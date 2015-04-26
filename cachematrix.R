## Coursera, R Programming, April 2014
## Programming Assignment 2
## Those two functions create an object that stores a matrix and cache it's inverse

## Creates the object that contains the matrix

makeCacheMatrix <- function(matrix = matrix()) {
  invMatrix <- NULL
  set <- function(y){
    matrix <<- y
    invMatrix <<- NULL
  }
  get <- function() matrix
  setInv <- function(inverse) invMatrix <<- inverse
  getInv <- function() invMatrix
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## Return a matrix that is the inverse of 'matrix'
cacheSolve <- function(matrix, ...) {
  invMatrix <- matrix$getInv()
  if(!is.null(invMatrix)){
    message("Getting cached Inverse Matrix")
    return (invMatrix)
  }
  data <- matrix$get()
  inv <- solve(data, ...)
  matrix$setInv(inv)
  inv
}
