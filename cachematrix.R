## These functions used together can demonstrate how lexical scoping rules in R can 
## be used to store (cache) data for future retrieval.  This can be useful for storing
## the results of computationally expensive calculations for speedy retrieval.

## The makeCacheMatrix function returns a list in an environment with functions which can be 
## used to set and get a matrix and its cached inverse.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function takes a special list returned by makeCacheMatrix as an argument 
## and either returns a matrix's cached inverse or if its not yet been calculated, calculates 
## it and then stores it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

