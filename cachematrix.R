## There are two functions. The overall purpose is to find the inverse
## of a mtrix. But to save unnecessary computation, if the inverse
## has already been calculated, this should be spitted out from a 
## cache.



## The makeCacheMatrix will create a special "matrix" object that 
## can cache its inverse.
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


## cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated, then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(x)
  x$setinverse(m)
  m
  
}
