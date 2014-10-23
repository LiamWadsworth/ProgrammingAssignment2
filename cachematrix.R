## R programming assignment two

## This script has two functions
## 1) makeCacheMatrix which creates a matrix () and caches it
## 2) cacheSolve which computes the inverse of matrix () - this is done using the solve function in R

## Note: It is assumed in both functions that the matrix supplied will be square
## Note: Code has been based on the example coding on the assignment page

## 1) makeCacheMatrix which creates a matrix () and caches it

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL                    
  set <- function(y) {
    x <<- y                      
    m <<- NULL
  }
  
  get <- function() x			  
  matrixSet <- function(solve) m <<- solve    
  matrixGet <- function() m
  list(set = set, get = get,
       matrixSet = matrixSet,
       matrixGet = matrixGet)
}



## 2) cacheSolve which computes the inverse of matrix () - this is done using the solve function in R

cacheSolve <- function(x, ...) {
  inverse <- x$matrixGet()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$matrixSet(inverse)
  inverse
}