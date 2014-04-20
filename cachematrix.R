## These following two functions provide a way to cache the inverse of a matrix
## and provide the cached matrix inverse when necessary
## Changing the matrix will result in the lazy evaluation of the inverse
##
## It is assumed that the matrices used are invertible,  it may be
## useful to have a check to see if the matrix is invertible when
## it is being set
##
## Following code creates invertible matrices for testing purposes
## twobytwo = matrix(c(4,3,3,2), 2, 2)
## threebythree = matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)


##
## This function creates a list of functions to get/set the matrix and
## its inverse.
##
## Wondering if it is a better design to move the functionality from the cacheSolve
## function to the getinverse function here and drop the setinverse and cacheSolve
## functions altogether.
##
makeCacheMatrix <- function(x = matrix()) {
  imatrix <- NULL
  
  # function to change the matrix, cached instnace of the inverse is set
  # to null and is lazily evaluated
  set <- function(y) {
    if (!identical(x,y)) {
      # if matrices are different then mark the inverse as null
      # to force computation of the inverse when cacheSolve is invoked
      x <<- y
      imatrix <<- NULL
    }
  }
  
  # function to get the matrix
  get <- function() x
  setinverse <- function(i) imatrix <<- i
  getinverse <- function() imatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function takes the special matrix created using the above
## method and returns the inverse of the matrix
## matrix inverse is calculated and cached if it is not already cached
cacheSolve <- function(x, ...) {
  minverse <- x$getinverse()
  
  ## if the matrix has changed
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}
