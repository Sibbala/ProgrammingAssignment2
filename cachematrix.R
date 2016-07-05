##Special matrix willbe created by the function makeCacheMatrix, This will also can cache its inverse using the solve inbuilt function
##get function returns the vector x stored in the main function. set function changes the vector stored in the main function.
##setmean and getmean are functions very similar to set and get.They don’t calculate the mean, they simply store the value of the input in a variable m.
## into the main function makeVector (setmean) and return it (getmean).
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve function calculates the inverse of the special “matrix” returned by makeCacheMatrix .
##If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
##If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse,
##and x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting the cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

        ## Return a matrix that is the inverse of 'x'
        eg: > matrx <- matrix(1:4,2)
> matrx
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> CachedMarix <- makeCacheMatrix(matrx)
> cacheSolve(CachedMarix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
## this is the inverse of the input matrix

CachedMarix <- makeCacheMatrix(matrx)
cacheSolve(CachedMarix)


