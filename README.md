## put comments here that gives an overall description of what your
## function does

##There are two function makeCacheMatrix.makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv,getinv

##This function works best with squared matrices


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL            #initializing inverse as NULL
  set <- function(y){
         x <<- y
         inv <<- NULL
  }
  get <- function() {x}      #function to get matrix x
  setInverse <- function(inverse)  {inv <<- inverse}
  getInverse <- function() {inv}     #function to obtain inverser of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## write a short comment describing this function
##This is used to get cache data

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
        message("getting cached data")
        return(inv)
  } 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}










