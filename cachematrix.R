## This script contains two functions that can be used to 
## implement a cached version of the solve() function. 
## The first function, makeCacheMatrix() sets up the data 
## structure necessary to handle the matrix and cache data.
## The second function, cacheSolve() works like the solve()
## function to return the inverse of its matrix argument,
## but if it has already done the calculation before, it 
## simply retrieves the stored answer from the data structure.

## Given a matrix, m, creates a list of functions that reads 
## and writes to the matrix and its inverse
makeCacheMatrix = function(m = matrix()) {
  ## Initialize i to NULL every time a new cache matrix is made
  i = NULL 
  
  ## Four functions to handle reading and writing to the list
  set = function(y) {
    m <<- y
    i <<- NULL
  }
  get = function() m
  setInv = function(inv) i <<- inv
  getInv = function() i
  
  ## Return list
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## Uses the list from makeCacheMatrix to see if an inverse 
## has previously been calculated and stored. If the cached 
## inverse exists, return that. Otherwise, calculate it, 
## store it as cache, and return it.
cacheSolve = function(m, ...) {
  ## If the mean is already stored in cache: retrieve and return
  i = m$getInv()
  if (!is.null(i)) {
    print("Getting cached data...")
    return(i)
  }
  
  ## If mean has not been cached: calculate, store, return
  data = m$get()
  print("Calculating afresh...")
  i = solve(data, ...)
  m$setInv(i)
  i
}