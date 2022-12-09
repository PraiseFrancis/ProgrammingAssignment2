## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ## To obtain the inverse of the matrix 'x'
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function is used to get the cache data
cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {   ## To check whether the inverse is NULL
    message("getting cached data")
    return(inv)           ## returns the inverse value
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...) ##  function calculates inverse value
  x$setinverse(inv)       ## returns matrix inverse of 'x'
  inv
}
