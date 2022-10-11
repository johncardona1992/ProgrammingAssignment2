## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function receives a matrix as a parameter a returns a list of four functions (two setters and two getters)
## one setter and getter for the matrix attribute and the other two for the inverse attribute.
makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  set <- function( matrix ) {
    matriz <<- matrix
    result <<- NULL
  }
  get <- function() {
    matriz
  }
  setinverse <- function(inverse) {
    result <<- inverse
  }
  getinverse <- function() {
    result
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## this function receives the object returned by makeCacheMatrix() and determines if the inverse is already calculated
## in that case the function returns the cached inverse, otherwise it is calculated and save in cache memory.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matriz <- x$getinverse()
  if( !is.null(matriz) ) {
    message("getting cached data")
    return(matriz)
  }
  data <- x$get()
  matriz <- solve(data) %*% data
  x$setinverse(matriz)
  matriz
}

test <- makeCacheMatrix()
test$set(matrix(1:4, 2))
test$get()
cacheSolve(test)
cacheSolve(test)
