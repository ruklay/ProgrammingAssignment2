## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    invertedMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  get <- function() x
  
  setInvertedMatrix <- function(matrix) invertedMatrix <<- matrix
  getInvertedMatrix <- function() invertedMatrix
    
  list(set = set, get = get,
       getInvertedMatrix = getInvertedMatrix,
       setInvertedMatrix = setInvertedMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invertedMatrix <- x$getInvertedMatrix()
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  
  data <- x$get();
  invertedMatrix <- solve(data)
  x$setInvertedMatrix(invertedMatrix)
  invertedMatrix
}
