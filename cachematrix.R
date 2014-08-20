## makeCacheMatrix and cacheSolve are two functions that are used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix" object which contains a list consisting of functions to:
## 1) set the value of the matrix, 2) get the value of the matrix, 3)set the value of its inverse 4)get the value of its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(solve1) i<<-solve1
  getinverse<-function() i
  list(set = set, get=get, 
       setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix and stores this value in the cache using the setinverse function.
## However if the inverse has already been calculated it gets this value from the cache and skips calculations.

cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
