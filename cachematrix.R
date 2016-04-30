## Coursera 'R-programming' assignment 2

## This set of functions will create the inverse of a matrix
## and store the result in cache. The function will check 
## whether the inverse of the matrix already exists and in 
## that case retrieves the 'answer' from cache to prevent 
## recalculation.

## The makeCasheMatrix() function creates a list of functions 
## for the input matrixt hat can be passed into the cacheSolve() 
## function

makeCacheMatrix <- function(x = matrix()) {
  ## create a list of functions to set & get the matrix
  ## and to set & get the inverse
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## The casheSolve() function computes the inverse of the 
## matrix, using the makeCasheMatrix() function. It also checks 
## whether the cache already contains the answer 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  ## first check if the inverse matrix already exists
  ## if it does, then return the previously calculated inverse
  if(!is.null(inv)){
    message("Getting inverse matrix from cache")
    return(inv)
  }
  ## if the inverse doesn't exist already, it is calculated
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  message("Calculating inverse matrix:")
  print(inv)
  message("Inverse matrix stored in cache")
}
