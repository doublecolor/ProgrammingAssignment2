## Put comments here that give an overall description of what your
## functions do

## the file contains two functions makeCacheMatrix and 
## cacheSolve. They cache the inverse of a matrix.

## Write a short comment describing this function 

## makeCacheMatrix creates a special matrix object. 
## It cache the input matrix's inverse values.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function
## cacheSolve calculates the inverse of a matrix, using makeCacheMatrix function
## If the inverse already calculated and input not changed,  
## cacheSolve retrieve inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}