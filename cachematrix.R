## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialising the inverse to be NULL
  m<-NULL
  
  ## Defines a function that sets the matrix x to a new matrix y 
  ## And resets the inverse, m, to NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ## Defines a function that returns the matrix x 
  get<-function() {
    x
  }
  
  ## Defines a function that sets the inverse m to solve
  setmatrix<-function(solve) {
    m<<- solve
  }
  
  ## Defines a function that returns the inverse m
  getmatrix<-function() {
    m
  }
  
  ## Creates a list that contains the four functions defined above 
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  
  ## First check if the inverse has already been calculated
    if(!is.null(m)){
    message("getting cached data")
    return(m)
    ## If so, get the inverse from the cache and skip the computation
  }
  matrix<-x$get()
  ## Otherwise, it calculates the inverse 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}