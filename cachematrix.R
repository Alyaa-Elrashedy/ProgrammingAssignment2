## These functions create a special matrix object and cache its inverse 
## to avoid redundant, costly computation.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  ## Reset cache when the matrix changes
  }
  
  ## Method to get the matrix
  get <- function() x
  
  ## Method to set the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## Method to get the inverse matrix
  getInverse <- function() inv
  
  ## Return a list of the internal functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Check if the inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix data
  data <- x$get()
  
  ## Calculate the inverse using solve()
  inv <- solve(data, ...)
  
  ## Cache the calculated inverse
  x$setInverse(inv)
  
  ## Return the inverse
  inv
}