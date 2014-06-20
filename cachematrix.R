## makeCacheMatrix creates a special "matrix" objcet, using the matrix passed to it, 
## and keeps a copy of the its inverse in it. It returns a list which exposes the 
## functions to get & set the matrix, and get/set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  invertedMatrix <- NULL
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invertedMatrix <<- inverse
  getinverse <- function() invertedMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This methods calculates the inverse of a matrix. It takes a special "matrix" onbject
## and gets the cached inverse from this "matrix" object. If this method is used for the
## fiorst time on the special "matrix" object, it calculates the inverse, and caches
## the inverse in the "matrix" object for future calls

cacheSolve <- function(x, ...) {
  invertedMatrix <- x$getinverse()
  if(!is.null(invertedMatrix)) {
    message("getting cached matrix inverse")
    return(invertedMatrix)
  }
  data <- x$get()
  invertedMatrix <- solve(data)
  x$setinverse(invertedMatrix)
  invertedMatrix
}
