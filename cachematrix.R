# Coursera
# Data Science Specialization
# 02 R Programming
# Week 3, Assignment 2
# 2014-07-25

## Assignment 2: 
## create a function to emulate the caching/computation of a matrix 
## through two functions.  


## makeCacheMatrix:
##  -- initializer function; uses the get/set pattern

makeCacheMatrix <- function(x = matrix()) {
  
  # x is the current matrix  
  # m is the inverse matrix (initially NULL)
    
  m <- NULL  

  # four 'public' methods:
  # set: set the object's 'x' to a new matrix (and also clear the inverse!)
  # get: return the object's 'x'
  # setINverse: set the object's 'y' to the calculated inverse
  # getInverse: return the object's 'y'... test this for NULL to see whether 
  #             no inverse has been set yet
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(new_inv) m <<- new_inv
  getInverse <- function() m
  
    # initializer: returns a list of items
  list ( set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Caveats:
##  * Computing the inverse of a square matrix can be done with the solve 
##    function in R. For example, if X is a square invertible matrix, then 
##    solve(X) returns its inverse.


## cacheSolve:
##  -- purpose: returns inverse of matrix
##  -- method: first, check the 'cachematrix' to see if inverse exists;
##      if inverse exists, then return it; if inverse not exist, then calculate

cacheSolve <- function(x, ...) {

  ## PRE-CONDITION: 
  ##  * For this assignment, assume that the matrix supplied is always invertible
  ## POST-CONDITION:
  ##  * Return a matrix that is the inverse of 'x'

    # t = test value; see if X already has an inverse cached
  t <- x$getInverse()

    # if t is !null, then an inverse has been created/cached previously 
    # through cacheSolve, so no need to solve again.  Return the cache and move on.
  if( !is.null(t)) {
    message("using cached inverse")
    return(t)
  }
  
    # else, when it gets to here, then calculate inverse & store it in cache
  data <- x$get()
  t <- solve(data, ...)
  x$setInverse(t)
  
  t
}
