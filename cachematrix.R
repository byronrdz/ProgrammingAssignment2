#The functions makeCacheMatrix() and cacheSolve() can be used in conjunction to calculate the 
#inverse of a matrix and to maintain its result in an special variable in their parent context.
#These functions profit the Lexical context of R to maintain the status of object-type variables
#avoiding the necessity to calculate the inverse of the matrix each time it is required, 
#which implies an important reduction in the computational load of the system.

#MakeCacheMatrix(matrix)
##This function creates a matrix-type object which contains the functions to set and get the matrix
##and its inverse. Its only argument is a matrix, which must be square and have a valid inverse.
##The "object" generated can be stored in a variable to be passed to another function.
##The operator "<<-" stores the "properties" of the object in the parent context of the function.

makeCacheMatrix <- function(mtrx = matrix()) {
          ##Returns a 'matrix-type object' which contains the matrix itself and his inverse,
          ##if it has already been calculated.
          ##Furthermore it contains functions to set and get their properties.
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


#cacheSolve(matrix-type-object)
##This function calculates the inverse of the matrix contained in the "matrix-type object". But first
##examines if the inverse has already been calculated. If so it simply returns the stored value.
##If the inverse is null then it calculates his value. This function uses the functions of the
##matrix-type-object to get the matrix and to set his inverse. 
##The argument of this fuction is the matrix-type object, but it can also take the arguments of the
##function "solve()".

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
