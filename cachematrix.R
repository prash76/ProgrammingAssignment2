## makeCachematrix stores a matrix and its inverse.

## function below stores matrix and its inverse and has methods for setting and getting them.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <-  NULL
  
  set <- function(y){
    
    x <<- y
    inverse <<- NULL
    
  }
  
  get <-  function() x
  
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}


## function below takes an list returned by makeCacheMatrix() and then calculates the
## inverse of matrix inside and updates the list.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  matrix1 = x$get()
  inverse1 = x$getinverse()
  
  if(!is.null(inverse1)){
    
    message("getting cached data")
    return(inverse1)
    
  }
  
  inverse1 = solve(matrix1, ...)
  x$setinverse(inverse1)
  inverse1
  
  
  
}
