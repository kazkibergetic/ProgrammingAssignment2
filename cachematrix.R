## Below are two functions that are used to create an inverse if the provided matrix
## and caches its inverse. 

## The futest <- makeCacheMatrix(y)nction creates a special "matrix" object that stores an original matrix and 
## caches the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
   set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) inverse <<- mean
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function finds the inverse of the matrix.
## However, it first checks to see if the inverse matrix has already been created
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinverse function.

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
