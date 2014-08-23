# Matrix operations are costly and can benefit from caching
# This is a reusable component to cache the inverse of a given matrix

# makeCacheMatrix function that holds a list of the following functions:
# * Sets the value of the cache matrix
# * Gets the value of the matrix
# * Sets the value of inverse matrix
# * Gets the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL #set the inverse matrix to null
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inverseMatrix <<- inverse
  
  getinverse <- function() {
    inverseMatrix #return the inverse matrix
  }
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The directions are to assume that the matrix is always inversible. So no additional validations are necessary.

# The following function returns the inverse of a given matrix.
# It first checks the cache to see if inverse is already computed. 
#   If the cache exists, it gets the result and skips computation.
#   If not, it computes the inverse and sets the value in the cache for next time.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("found cache. returning the cached data.")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  inverseMatrix
}