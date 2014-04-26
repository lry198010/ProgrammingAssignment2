## This R source code have 2 functions that using cache to storage the inverse of a 
## matrix once the inverse was calculate. 
## In order to used the cacheSolve function, you first need to formated the matrix 
## to sMatrix using makeCacheMatrix function.

## Create a special Matrix Object (named sMatrix) for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y){
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(I) Inv <<- I
  getInverse <- function() Inv
  list(get=get,set=set,setInverse=setInverse,getInverse=getInverse)
}


## compute the Inverse of a sMatrix using cache mechnism

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("get inverse by cache")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv  
}
