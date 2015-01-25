## makeCacheMatrix creates a list containing a function to
## set the value of the matrix
## get the value of the vector
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## The following function returns the inverse of the matrix. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the result and skips the computation. 
## Otherwise, it calculates the inverse and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
