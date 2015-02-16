################################################################################
## makeCacheMatrix(x) - creates a Matrix list-object that contains a closure
##  that caches in inverse of the matrix.
## ARGS: x is the initial matrix.  It must be invertable.  If not, attempting
##  to invert will create a crack in time and space.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # The inverse.  Initially empty
  set <- function(y) { # set is a closure, capturing x, which holds the matrix
    x <<- y
    i <<- NULL  #set changes x, so any cached value of inverse must be dumped
  }
  get <- function() x
  #getinverse and setinverse should only be used in cacheSolve.  Would be made private in
  #a proper OO systetm
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    return(i)
  }
  theMatrix <- x$get()
  i <- solve(theMatrix)
  x$setinverse(i)
  i
}
