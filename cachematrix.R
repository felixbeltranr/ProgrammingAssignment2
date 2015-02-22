## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## RUN EXAMPLE
## x <- makeCacheMatrix(matrix( rnorm(16), nrow=4, ncol=4))
## x$get()
## cacheSolve(x)
## cacheSolve(x) ##This time we get the "getting cached data" message
## x$set(matrix( rnorm(16), nrow=4, ncol=4))
## cacheSolve(x)
## cacheSolve(x) ##This time we get the "getting cached data" message
