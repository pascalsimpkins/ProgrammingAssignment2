
## Gradeable functions are below

## Creates an inverse cacheable matrix
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


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if (!is.null(inv)) {
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}

matrix_1 <- makeCacheMatrix(matrix(1:4, 2, 2))
matrix_1$get()
cacheSolve(matrix_1)
matrix_1$getInverse()
