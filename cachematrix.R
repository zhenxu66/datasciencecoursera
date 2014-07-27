## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## similar with the example, functions set and setinverse upload data to the parent 
## function and leave m default as NULL
## Functions as get and getinverse just output result

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    if (identical(x,y)) return
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## getinverse understand if NULL or not, solve to get the inverse matrix

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}