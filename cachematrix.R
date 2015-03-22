## Below is a pair of functions that cache the inverse of a matrix to save computation time 
## if the inverse of the matrix exists.
## 3rd function is for testing purpose

## This function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## If the inverse has not been calculated, it calculates the inverse of the matrix and sets the value of
## the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}

## testing the above functions, see if caching really makes a difference for large dataset
## type the following in console after running cachematrix.R
## a <- matrix(rnorm(4000000), 2000, 2000)
## testscript(a)
testscript <- function(mat){
  ## call makeCacheMatrix
  newmat <- makeCacheMatrix(mat)
  
  ## starting time of try 1
  begin <- Sys.time()
  ## call cacheSolve
  solution1 <- cacheSolve(newmat)
  dur <- Sys.time() - begin
  message("try1")
  print(dur)
  print(solution1[1:8,1:8])
  
  ## starting time of try 2
  begin <- Sys.time()
  ## call cacheSolve
  solution2 <- cacheSolve(newmat)
  dur <- Sys.time() - begin
  message("try2")
  print(dur)
  print(solution2[1:8,1:8])
}