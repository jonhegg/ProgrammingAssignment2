
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ##Function for storing data
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  ##Return the function as a list
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrixx 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    ##Here we are using the cached data
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  
  ##Inverting the data
  i <- solve(data)
  
  ##Store the inverted data in cache
  x$setinverse(i)
  i
}
