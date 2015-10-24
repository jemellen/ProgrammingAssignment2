## These functions will take a square matrix and return the inverse of that matrix 
## A special cached matrix is used to speed computation when the matrix has not changed

## makeCacheMatrix excepts a matrix and returns an object containing four functions (getters/setters)
## the object can store both the original matrix and the computed inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # store original maxtix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # return original matrix 
  get <- function() x
  
  # store inverse matrix
  setInvMatrix <- function(ma) m <<- ma
  
  #return inverse matrix 
  getInvMatrix <- function() m
  
  # return functions contained in object along with original matrix value
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## cacheSolve takes a CacheMatrix object (a matrix) and returns the inverse of that maxrix
## if the matrix has already been cached, it returns the cache, otherwise it uses Solve to 
## establish the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInvMatrix()
  
  # is the value already cached? Returned cahced value 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # get the original matrix, compute the inverse, and store it back to the object
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  
  # should return inverse of matrix
  m
}
