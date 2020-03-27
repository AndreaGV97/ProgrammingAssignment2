## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversematrix<-NULL
  #Now we set a value to a Matrix 
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x      #get the value of the matrix
  setinverse <- function(inverse) inversematrix <<- inverse #set the value of a matrix that has to be ivertible
  getinverse <- function() inversematrix #get the value of the matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("obteniendo datos")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
B <- matrix(c(6,8,3,5),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
cacheSolve(B1)
