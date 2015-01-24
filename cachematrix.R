## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mtrx = matrix()) {
  mtrx_inv <- NULL
  set <- function(y) {
    mtrx <<- y
    mtrx_inv <<- NULL
  }
  get <- function() mtrx
  setinverse <- function(m_i) mtrx_inv <<- m_i
  getinverse <- function() mtrx_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(mt, ...) {
        ## Return a matrix that is the inverse of 'mt'
  mt_inv <- mt$getinverse()
  if(!is.null(mt_inv)) {
    message("getting cached inverse matrix")
    return(mt_inv)
  }
  matriz <- mt$get()
  mt_inv <- solve(matriz, ...)
  mt$setinverse(mt_inv)
  mt_inv
}
