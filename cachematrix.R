## Functions that cache the inverse of a matrix, or at
## least an attempt at that

## Set value of the matrix
## get value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <-function(y){
    x<<- y
    i<<- NULL
  }
  get <-function() x
  setinverse <-function(inv) i <<- inv
  getinverse <function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## calculate the inverse of the special "matrix" 
## created by above function, reusing cached result
## if applicable

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m,...)
  x$setinverse(i)
  i
}