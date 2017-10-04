## These functions call the inverse of a desired matrix and prints the inverse to memory to 
## decrease operational time

## This function stores the inverse of a desired matrix to memory or a cache

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
  get=function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function returns the inverse of a matrix 

cacheSolve <- function(x, ...) {
  
  inv = x$getinv
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
