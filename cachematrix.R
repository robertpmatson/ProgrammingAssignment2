## Put comments here that give an overall description of what your
## functions do

## This will create a 'vector' that has methods to set and get a matrix
## and provide the variable for the inverse of the matrix 'x', 
## which is cached in 'm'
## The methods to get and set the variable holding the 

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, 
         setinverse = setinverse,
         getinverse = getinverse)
  
}

## This will return a matrix that is the inverse of 'x'x, returning x from the 
## cache if it is already available in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  data <- solve(data)
  m <- data
  x$setinverse(m)
  m
}

# Usage:
# Download cachematrix.R to your current working directory
# Call: source("cachemarix.R")
# Call: x <- makeCacheMatrix()
# Create a matrix, 
# Call: a <- matrix(as.numeric(1:4), ncol=2, nrow=2) 
# Call: x$set(a) - Set the matrix in x
# Call: cacheSolve(x)
# This time you will see the message that the cached matrix is being used
# Call: cacheSolve(x) 
# Set a new matrix in 'x'. I used a new matrix here but 'a' could be passed to set without change
# a <- matrix(as.numeric(11:14), ncol=2, nrow=2)
# x$set(a)
# Call: cacheSolve(x)
# This time you will see the message that the cached matrix s being used
# Call: cacheSolve(x)