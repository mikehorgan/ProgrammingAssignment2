## 
## makeCacheMatrix.R - implementation of a matrix object with inverse matrix caching
##
## Mike Horgan  2018-02-12

##
## CacheMatrix constructor function - return a new CacheMatrix object built from the 
##   'x' matrix argument. As defined, X is assumed to be an invertible matrix.
##

makeCacheMatrix <- function(x = matrix()) {
  # initialize cached version
  x_inverse <- NULL
  
  # setter function
  set <- function(new_matrix) {
    x <<- new_matrix
    x_inverse <<- NULL
  }
  
  # getter function
  get <- function() {
    x
  }
  
  # setinverse method - update cached inverse matrix
  setinverse <- function(inverse) {
    x_inverse <<- inverse
  }
  
  # getinverse method - return cached inverse matrix
  getinverse <- function() x_inverse
  
  # return method list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - return cached matrix inverse for CacheMatrix x

cacheSolve <- function(x, ...) {
  
    # Check for cached version; if not found calculate it and update cache
    if (is.null(x$getinverse())) {
          source_matrix <- x$get()
          inverted_matrix = solve(source_matrix)
          x$setinverse(inverted_matrix)
    }
    
    # return cached inverse
    x$getinverse()
    
}
