## The first function takes in x (an invertible matrix) as input. The matrix is created so it can cache its inverse.
## The second function is the actual calculation of the inverse and hence returns the inverse of the matrix x.
## However, it first checks if the inverse has been cached already, and if so, it returns the cached inverse thus saving computation time.


## Initial input x is an invertible matrix. "inv" is set by default as null. 
## "Set" redefines Matrix x as matrix y and inv is reset as a null.
## "get" returns x 
## "setinverse" is a function which takes inv as an argument 
## "getinverse" returns inv which will be defined in function 2 as the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes in an invertible matrix x
## inv is defined as "getinverse"
## If inv is not null it means the matrix inverse has been calculated and cached as inv exists stored in the above function
## Hence the message "getting cached inverse" is returned. And inv is returned as it is
## Otherwise, the matrix x is retrieved and inv is defined as Solve(matrix, ...) which gets the inverse of x
## Inv is returned in that case after it has been defined as the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
      }
  matrix <- x$get()
  inv<- solve(matrix, ...)
  x$setmatrix(inv)
  inv
}
