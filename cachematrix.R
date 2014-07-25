## Put comments here that give an overall description of what your
## functions do
## makeCacheMAtrix and cacheSolve are functions that uses matrix and solve 
## functions in R to make a function that caches the inverse of a matrix
## once it is solved.

## Write a short comment describing this function
## This function is something similar to a class, you have to 
## "instantiate" it in a variable or symbol, and then access its inner 
## functions as getters and setters.
## set() gives the value to the matrix, receiving a matrix as parameter


makeCacheMatrix <- function(x = matrix()) {

  inverse<-NULL
  set <- function(mtx) {
    internmatrix <<- mtx
    inverse <<- NULL
  }
  get <- function() internmatrix
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## Write a short comment describing this function
## cacheSolve receives a symbol of makeCacheMatrix, and if the inverse
## matrix has already been calculated, then returns the value.
## If not, it calculates and asigns to the inverse value of makeCacheMatrix
## and returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
