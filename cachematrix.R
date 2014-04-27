## Functions makeCacheMatrix() and cacheSolve() - tested with the following commands.
## more checks should be included in real environment. But these are per assignment.
## t <- makeCacheMatrix(matrix(1:6,3))       # check not square matrix.
## t$set(matrix(1:6,3))
## t$get()
## t <- makeCacheMatrix(matrix(5:8,2))
## t$set(matrix(5:8,2))
## t$getinverse()
## cachematrix(t)
## t$getinverse()
## t$get()
## cachematrix(t)
## g <- makeCacheMatrix(matrix(5:8,2))
## t$get()
## g$get()
## g$getinverse()
## cachematrix(g)
## cachematrix(g)
## source('~/makeCacheMatrix.R')


## makeCacheMatrix() has 4 functions set, get, setinverse, getinverse. 
## 1. set() function caches and also checks if it is a square matrix, and gives a friendly message if its not. 
##    it could include stop or exit statement in real world. 
## 2. when getinverse() is called first time, before cacheSolve, it reminds you to do so, where it checks determinant.


makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) { 
    if(nrow(y)!=ncol(y)) {
  s <- sprintf("%d rows & %d cols, not a square matrix, not invertible, try another matrix.", nrow(y), ncol(y))
  print(s)
    }
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(x) m <<- x 
  getinverse <- function() {
    
    if(!is.null(m)) {m}
    else {message("cacheSolve(m) for first time")
    }
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## cacheSolve() returns inverse matrix if it exists, otherwise checks if invertible and returns the inverted matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  detM <- det(data)
  if(detM==0) {
    message("Matrix cannot be inverted, try another square matrix")
  }
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}

