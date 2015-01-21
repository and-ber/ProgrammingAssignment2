## The two functions cache the inverse of a matrix, instead of repeating its calculation more than once

## "makeCacheMatrix" creates a vector, which is a list containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invsolve) inv <<- invsolve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse matrix of the special "vector" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets such matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets this matrix in the cache via setinv

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
