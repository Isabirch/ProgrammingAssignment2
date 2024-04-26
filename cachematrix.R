
## Create a matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(y) inv <<- y
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Retrieve the inverse matrix if already solved, otherwise solve and return the 
## new solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) return(i)
  
  ## if we got here, the inverse hasn't already been cached
  mat <- x$get()
  inv <- solve(mat, ...) #note that additional options are not cached
  x$setinverse(inv)
  inv
}
