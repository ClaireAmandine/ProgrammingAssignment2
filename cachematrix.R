# This function creates a special "matrix" object that can cache its inverse #

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse


makeCacheMatrix <- function(X = matrix()) {
  Inv <- NULL
  set <- function(Y) {
    X <<- Y
    Inv <<- NULL
  }
  get <- function() X
  setinverse <- function(solve) Inv <<- solve
  getinverse <- function() Inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of special "matrices" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.
# Otherwise it will calculate the inverse of the matrix.
# This prevents to recompute the inverse of a given matrix repeatedly and can save computational time.


cacheSolve <- function(X, ...) {
  Inv <- X$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- X$get()
  Inv <- solve(data, ...)
  X$setinverse(Inv)
  Inv
}
