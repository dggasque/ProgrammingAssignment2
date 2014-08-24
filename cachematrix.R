## makeCacheMatrix creates a special matrix object that can cache its inverse
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<-inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  } else {
    m <- solve(x$get())
    x$setinverse(m)
    return(m)
  }
}