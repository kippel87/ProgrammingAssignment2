## creates a special "matrix" object that can cache its inverse
## x: Matrix
## Cinv: Cache inverse of x
## set(x): Function to store the matrix x
## get() : Returns the matrix x
## setinv(inv): Function to store the inverse inv in the cache
## getinv(): Returns the inverse from the cache

makeCacheMatrix <- function(Cx = matrix()) {
  Cinv <- NULL
  set <- function(y) {
    Cx <<- y
    Cinv <<- NULL
  }
  get <- function() Cx
  setinv <- function(inv) Cinv <<- inv
  getinv <- function() Cinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## calculates the inverse of the special "matrix" created with tmakeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
