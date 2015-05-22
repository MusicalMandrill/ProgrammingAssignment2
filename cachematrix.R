## Paired functions that calculate, cache, and retrieve the
## inverse of a square invertible matrix 'X'
##
## makeCacheMatrix(X)
## Args: 'X' where X is a square invertible matrix

## Function to create and store a list of functions that:
##   F1. Set the value of the list of functions
##   F2. Get the value of the list of functions
##   F3. Set the value of the inverse of X
##   F4. Get the value of the inverse of X

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                                  #Initializes variable 'inverse'
  set <-function(y) {                              #F1. (Re)sets function list 
    x<<-y
    inverse<<-NULL
  }
  get <- function(){x}                             #F2. Returns function list
  setinverse <-function(solve) inverse<<-solve     #F3. Calculates/stores inverse of X in 'inverse'
  getinverse <- function() inverse                 #F4. Retrieves value of 'inverse'     
  list(set = set,get = get,                        #Labels and stores functions in list
       setinverse = setinverse,
       getinverse =getinverse)
}


## cacheSolve(Z)
## Args: Output of function makeCacheMatrix(X)
## Example Arg: cacheSolve(Z<-makeCacheMatrix(X))

## Returns inverse of matrix 'X' by calling makeCacheMatrix subfunctions as needed to:
##   f1. Retrieve inverse of matrix 'X'
##   f2. Calculate and store inverse of matrix 'X'
##   f3. Return inverse of matrix 'X'

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()                          #f1. If matrix inverse !NULL,retrieves cached inverse 
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()                                  #f2. Caculates and stores matrix inverse
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse                                          #f3. Returns matrix inverse
}
