
##  The two functions below are used to create a special object that stores a matrix and cache's its inverse value.

## The function `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function returns a matrix that is the inverse of 'x'. 
## It first checks to see if the inverse of 'x' has already been calculated. 
## If so, it `get`s the result from the cache and skips the computation. 
## Otherwise, it calculates the inverse of 'the data 'x' 
## and sets the inverse value in the cache via the `setinverse` function.
cacheSolve <- function(x, ...) {
        
  ## Checking if inverse has already been calculated
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Calculating the inverse value
  data <- x$get()
  inverse <- solve(data, ...)
  
  ## Setting the inverse value, so that it can be used later.
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}

