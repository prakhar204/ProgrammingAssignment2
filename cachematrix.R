## Created a function to create a matrix and would be able to cache the inverse
## Following suit to the the example makeCacheMatrix is a list which has the following functions
## set() to set the values of the matrix
## get() to get/display the value of the matrix
## setinverse() which takes the inverse as an input and caches the inverse
## getinverse() returns the inverse or NULL if inverse has not been set

## the funtion takes the matrix as an input or creates a matrix by default and has the above functions to pefrform the needed functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The below function takes an object of the special matrix created above
## If the inverse is not NULL it will display "Getting cached data and display the cached inverse
## If the inverse is not cached it will use the function solve and trigger the function setinverse() to set the inverse in the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)   ##Using solve function to get the inverse
  x$setinverse(i)
  i
}
