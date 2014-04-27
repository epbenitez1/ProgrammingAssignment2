## makeCacheMatrix: return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ##i will store the cached inverse matrix
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  ## Setter for the inverse
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  

}


##cacheSolve: Compute the inverse of the matrix. If the inverse is already
## calculated before, it returns the cached inverse.


  cacheSolve <- function(x, ...) {
    i  <- x$getinverse()
    
    ## If the inverse is already calculated, return it
    if (!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data  <- x$get()
    i  <- solve(data, ...)
    x$setinverse(i)
    i
  
 
  ## Return a matrix that is the inverse of 'x'
}
        
