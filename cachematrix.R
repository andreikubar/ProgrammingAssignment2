## These functions are meant to store a matrix and
## cache it's inverse
## to use run: m <- makeCacheMatrix ( <some_matrix> )
## to get the inverse: cacheSolve (m)

## This function creates an object holding a matrix and it's inverse
## and providing 4 methods: set, get, setsolve, getsolve  
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function (y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function ()  {
    x
  }
  
  setsolve <- function (y) {
    s <<- y
  }
  
  getsolve <- function () {
    s
  }
  
  list (set = set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function is used to retrieve the inverse of the object
## It takes a "makeCacheMatrix" as it's argument and returns the 
## inverse of the matrix stored in that object
## Inverse is only computed first time and then cached value is returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s<-x$getsolve()
  if (!is.null (s) ) {
    message ("returning cached value")
    s
  }
  else {
    s<-( solve (x$get(),...))
    x$setsolve(s)
    s
  }
}

