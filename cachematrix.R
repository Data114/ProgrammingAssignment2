## 1. 'set' function sets the value of the matrix.
## 2. 'get' function obtains the value of the matrix entered.
## 3. 'set_inverse' function calculates inverse of entered matrix and sets the function to that value.
## 4. 'get_inverse' function obtains value of the inverse of the entered matrix.

## The below function, 'makeCacheMatrix', creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {         # 1. setting the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x          # 2. getting the value of the matrix
  set_inverse <- function(inverse) i <<- inverse       # 3. setting the value of the inverse of matrix
  get_inverse <- function() i          # 4. getting the value of the inverse of the matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The below function, 'cacheSolve', computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, that is, when matrix hasn't changed then the 'cacheSolve' 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if (!is.null(i)) {
    message("Getting Cached Data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
