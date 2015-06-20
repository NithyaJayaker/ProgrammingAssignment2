##To write a pair of functions that cache the inverse of a matrix. 
##Using the <<- operator so that my function can cache the result and use cache value if present

##First function is "makeCacheMatrix"
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a=matrix()) {
  m <- NULL
  set <- function(y) {
    a <<- y
    m <<- NULL
  }
  get <- function() a
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##Second function is "cacheSolve"
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
  m <- a$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- a$get()
  m <- solve(data, ...)
  a$setinverse(m)
  m
}


##Below is the one of the input I used 
# i1<-makeCacheMatrix(matrix(c(1,4,9,0,-3,2,2,7,8),3,3))
# i1
#cacheSolve(i1)
