## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Caches the data into the cachealong with its inverse
makeCacheMatrix <- function(x = matrix()) {
  myInverse <- NULL
  set <- function(a) {
    x <<- a
    myInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(temp) myInverse <<- temp
  getinverse <- function() myInverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## returns inverse matrix from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myInverse <- x$getinverse()
  if(!is.null(myInverse)) {
    message("get data from the cache")
    return(myInverse)
  }
  w <- x$get()
  myInverse <- solve(w)
  x$setinverse(myInverse)
  myInverse
}
