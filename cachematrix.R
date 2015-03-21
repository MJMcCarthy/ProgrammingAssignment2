## We're creating a matrix like object, makeCacheMatrix, that caches its inverse and 
## a function, cacheSolve() to calculate that inverse. Once cacheSolve() calculates the inverses it stores it
## it in the object created by makeCacheMatrix.

##Here's my usage:
## mike <- matrix(c(1,-1,4, 2,0,3,6,1,1), 3,3)
## mat <-makeCacheMatrix(mike)
## cacheSolve(mat)
## mat$getInverse()

## Creates a matrix + object. The interesting thing about the object is it caches its inverse.
makeCacheMatrix <- function(m= matrix()){
  i <- NULL 
  set <- function(y){
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list( set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## cacheSolve() calculates teh inverse of the matrix used in a makeCacheMatrix object and then caches
##the inverse in that object.

cacheSolve <- function(m, ...) {
  i <- m$getInverse()
  if (!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- m$get()
  i <-solve(data)
  m$setInverse(i)
  i
}
