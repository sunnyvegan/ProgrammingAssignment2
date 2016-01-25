
## The function "makeCacheMatrix" stores a list of 4 functions and caches the
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## the "set" function resets the value of the matrix in the main fuction
  set <- function(y){
    x <<-y
    inverse <<- NULL
  }
  
  ## the function "get" returns the matrix that is stored in the main 
  ##function
  get <- function()x
  
  ## Setinverse resets the inverse of the matrix if it is not computed before.
  setinverse <- function(solve) inverse <<- solve
  
  ##getinverse caches the inverse of the matrix which is input of the main function
  getinverse <- function()inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## CacheSolve checks to for the cached inverse of a matrix and it is not
## found in cache, computes & returns the inverse.

cacheSolve <- function(x){

## Check the inverse of matrix stored in x and returns the inverse
  inverse <-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }

## If inverse is not cached, access the matrix stored in object x
## compute the inverse and cache the inverse in object x
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

