## The functions bellow can be used to compute and cache 
## the inverse of a given matrix.

## This function takes in a matrix and creates a special object
## that can be used to compute and cache inverse of the 
## provided matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes in the returned object of makeCacheMatrix 
## function, and returns the inverse of the matrix. If inverse already 
## cached it returns cached result othervise it computes inverse
## and caches the result.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}