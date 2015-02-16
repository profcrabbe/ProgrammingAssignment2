################################################################################
## cachematrix.R.  cache the inverse of a matrix for speedy access of repeated
##   calls.  contains two functions: makeCacheMatrix() that creates and stores
##   the actual matrix and its inverse, and cacheSolve() that does the inverting
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
  #getinverse and setinverse should only be used in cacheSolve.  Would be made 
  #private in a proper OO systetm
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  #construct the return value as a list, return it.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


################################################################################
##  cacheSolve(x,...) - takes a previously made "cacheMatrix" and returns its
##    inverse.  If the inverse was previously computed, this function just 
##    reurns the cached value, otherwise it also computes and stores the inverse
##    as well as returning it. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){  #if the inverse is cached, return it.
    return(i)
  }
  theMatrix <- x$get() #otherwise, compute and store.
  i <- solve(theMatrix)
  x$setinverse(i)
  i
}
