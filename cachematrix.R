#Assignment 2 - Caching the Inverse of a Matrix
#Matrix Inverse computation is costly. So, instead of calculating
#it repeatedly (if required), the invere an bbe cached also.
#This assignment has functions that cache the inverse of a square Matrix.

#This function will create a mtrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL
  
  set <- function(y){
    x <<- y
    iMatrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inv){
    iMatrix <<- inv
  }
  
  getInverse <- function(){
    return(iMatrix)
  }
  
  list(set = set , get = get, setInverse = setInverse, getInverse = getInverse)  
}


#This function returns the invere of matrix. If the inverse
#is cached, then it simply returns the cached copy else it 
#calculates the inverse of the matrix and store in cache.

cacheSolve <- function(x, ...) {  
  iMatrix <- x$getInverse()
  if(! is.null(iMatrix)){
    print("getting cached data!!!")
    return(iMatrix)
  }
  
  
  sMatrix <- x$get()
  iMatrix <- solve(sMatrix)
  x$setInverse(iMatrix)
  return(iMatrix)
}
