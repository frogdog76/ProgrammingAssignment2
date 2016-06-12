## These functions use lexical scoping to compute the inverse of an inversable matrix
## and store the solution in cache.  The first time the solution is computed the value 
## is stored in the function's environment. The next time called the solution is 
## retrieved from the function's environment rather than being recomputed.

## The makeCacheMatrix function takes a inversable matrix as an arg and creates a list 
## of functions that manipulate matrix vars in the local function environment:
## set() - Sets the value of the cached inversable matrix and resets the value of the 
##         cached inverse matrix to NULL
## get() - Gets the value of the cached inversable matrix
## setInvMatrix() - Sets the value of the inverse matrix in the cache (environment)
## getInvMatrix() - Gets the value of the inverse matrix from the cache (environment)

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(InvM) InvMatrix <<- InvM
  getInvMatrix <- function() InvMatrix
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## This function takes the function list returned by makeCacheMatrix to store the 
## matrix to be inversed and cache the solution of the inverse matrix operation. The first
## time the function is called the inverse of the matrix is computed and stored in the 
## cache.  The subsequent calls are retrieved from the cache instead of being recomputed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInvMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInvMatrix(inverseMatrix)
  inverseMatrix
}
