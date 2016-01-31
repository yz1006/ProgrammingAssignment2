## Create functions to cache the inverse of a matrix to save computing time and resource

## Define a function which creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  i <- NULL;
  set <- function(y=matrix()){
    x <<- y;
    i <<- NULL;    
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse;
  getInverse <- function() i;
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Define a function which returns the inverse of the matrix object created by function makeCacheMatrix
## If the inverse has been calculated, the cached inverse will be returned;
## if the inverse hasn't been calculated, this function will calculate and return inverse.

cacheSolve <- function(x,...){
  i <- x$getInverse();
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  };
  data <- x$get();
  i <- solve(data);
  x$setInverse(i)
  i
  
}
