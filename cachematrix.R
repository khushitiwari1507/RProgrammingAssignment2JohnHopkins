makeCacheMatrix <- function(matrix = matrix()) {
  inv <- NULL
  set <- function(mat) {
    matrix <<- mat
    inv <<- NULL
  }
  get <- function() matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# Create a cacheable matrix
mat_cache <- makeCacheMatrix(matrix)

# Compute inverse (and cache it)
cacheSolve(mat_cache)

# Retrieve cached inverse
cacheSolve(mat_cache)
