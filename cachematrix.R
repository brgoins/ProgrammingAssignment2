## makeCacheMatrix function puts a inverse matrix into the cache with the <<- command
## cacheSolve function uses solve function to the the inverse of the matrix

## puts inverse of x in the cache

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<-solve
  getmatrix<-function() m
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## sees if the matrix has already had the inverse calculated then pulls from cache
## if so, if not calculates and returns value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
