## makeCacheMatrix() creates a list of funtions that serve as the input for cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve() checks whether or not the inverse of the matrix has already been calculated
## This action relies heavily on the rules of S3 objects
## If the inverse has not already been calculated, solve() is employed to do so
## The solution is then added to the vector 'inv', an object in the makeCacheMatrix() env

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}