## Programming Assignment 2: Lexical Scoping
## Author: Luiz Fernando da Silva <folksilva@gmail.com>
##
## Example of use:
##
## > myMatrix <- rbind(c(1,2), c(2,1))
## > myCacheMatrix <- makeCacheMatrix(myMatrix)
## > cacheSolve(myCacheMatrix) # Here the inverse is computed and stored in cache
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > cacheSolve(myCacheMatrix) # Here the inverse is returned from cache
## Uhuu! It is from cache!
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## >
##

## Creates a matriz that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Compute the inverse of the matrix or get from cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Uhuu! It's from cache!")
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setinverse(i)
  i
}
