## Put comments here that give an overall description of what your
## functions do
##

## Write a short comment describing this function
##
## Function : makeCacheMatrix
## Desc     : This function creates a special 
##            "matrix" object that can cache its inverse.
##

makeCacheMatrix <- function(x = matrix()) {

  invMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  
  setInvMatrix <- function(matrix) invMatrix <<- matrix
  getInvMatrix <- function() invMatrix
    
  list(set = set, get = get,
       getInvMatrix = getInvMatrix,
       setInvMatrix = setInvMatrix)
}


## Write a short comment describing this function
##
## Function : cacheSolve
## Desc     : This function computes the inverse of 
##            the special "matrix" returned by 
##            the function makeCacheMatrix. 
##            If the inverse has already been calculated,
##            then it will retrieve the inverse from the cache
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInvMatrix()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  
  data <- x$get();
  invMatrix <- solve(data)
  x$setInvMatrix(invMatrix)
  invMatrix
}
