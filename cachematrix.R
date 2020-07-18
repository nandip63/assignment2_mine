## Put comments here that give an overall description of what your
## functions do
## These functions and this code are the solutions for the assignment 3 of R programming course

## Write a short comment describing this function
##This function helps create cache for inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                             ## initialize i as NULL;  
  set <- function(y) {                  ## define the set function  
    x <<- y                             ## value of matrix 
    i <<- NULL                          ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                   ## define the get fucntion - the matrix 
  
  setinverse <- function(inverse) i <<- inverse  ## value of inverse
  getinverse <- function() i                     ## get the value of inverse where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## Write a short comment describing this function
## This function uses the above function to retrieve inverse of a matrix from cache or create a new one

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("retrieving from  cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
