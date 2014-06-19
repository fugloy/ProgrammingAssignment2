## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this is a function meant to "hold" an inverted matrix
## the function takes a matrix as a parameter 
## and, in case the "instance" of this function doesn't
## already hold an inverted matrix, it computes the inverse of this matrix
## and "holds" the calculated matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invM) inv <<- invM
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## this function takes an instance of the "makeCacheMatrix"
## and checks if that instance "holds" an already inverted matrix
## if so, then that matrix is returned, otherwise the 
## inverted is calculated and returned


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
