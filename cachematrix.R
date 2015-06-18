## The makeCacheMatrix and cacheSolve functions together compute and cache the inverse of a matrix

## makeCacheMatrix creates a list of functions: set(), get(), setinverse(), and getinverse()

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
  
## cacheSolve computes the inverse of a matrix and caches that value

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

## test run
##x = rbind(c(1,3,3 ), c(1, 4, 3), c(1,3,4))
##m = makeCacheMatrix(x)
##m$get()
##[,1] [,2] [,3]
##[1,]    1    3    3
##[2,]    1    4    3
##[3,]    1    3    4
##cacheSolve(m)
##[,1] [,2] [,3]
##[1,]    7   -3   -3
##[2,]   -1    1    0
##[3,]   -1    0    1
