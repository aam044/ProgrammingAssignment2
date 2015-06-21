## These two functions support caching inverted matrix. 
## You need to initialize structure first by calling makeCacheMatrix 
## To get inverted matrix cachSolve function should be called
##
## Exaple of usage
## x<-matrix(1:9,3,3)
## x[1,]=c(4,6,11)
## y<-makeCacheMatrix(x)
## cacheSolve(y) # solving matrix
## ... do something ...
## cacheSolve(y)

makeCacheMatrix <- function(x = matrix()) {
## This function initialize structure to store original and cached matrix
## solved variable will contain solved matrix

  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) solved <<- solve
  getsolve <- function() solved
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  message("Start processing")
  
  solved <- x$getsolve()
  if(!is.null(solved)) {
    message("getting cached data")
    return(solved)
  }
  data <- x$get()
  solved <- solve(data, ...)
  x$setsolve(solved)
  solved  
}
