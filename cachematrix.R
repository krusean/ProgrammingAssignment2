##makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:

makeCacheMatrix <- function(x = matrix()) {
  
  #Initialize inverse 
  inverse <- NULL
  
  #Set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    
  }
  
  #Get the matrix
  get <- function() x
  
  #Set the inverse of the matrix
  setinverse <- function(i) inverse <<- i
  
  #Get the inverse of the matrix
  getinverse <- function() inverse
  
  #Return a list of methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #Assign inverse of x to variable inverse
  inverse <- x$getinverse()
  
  #If the inverse is already set, return it
  if (!is.null(inverse)){
    message("Cache")
    return(inverse)
  }
  
  #If not, calculate, set and return the inverse of the matrix
  message("Calculated")
  
  data <- x$get()
  
  m <- solve(data) 
  
  x$setinverse(m)
  
  m
  
}
