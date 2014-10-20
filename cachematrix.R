

## These are functions to use the function solve() to compute the inverse of a matrix
## The functions will save time by placing the results of the inverse into a cache
##  that can be called upon by the second function

## The makeCacheMatrix function will return a list of functions to set the value of the
##  matrix, get the value of the matrix, set the inverse of the matrix and get the
##  inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will check to see the inverse of a matrix has been calculated before 
##  it calls the solve() function. If the inverse has been calculated it will return
##  the results from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



