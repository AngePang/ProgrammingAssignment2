## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse)    inv <<- inverse
  getinv <- function()  inv
  return(list(set = set, get = get,
              setinv = setinv,
              getinv = getinv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  if (nrow(data) != ncol(data)) stop("matrix is invertible")
  if (abs(det(data))<1e-10) stop("matrix is singular")
  
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Just try to add some comments as practice in git...
