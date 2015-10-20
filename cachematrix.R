#This program will cache the inverse of a matrix under a parent function and return it if it is recalled. If the inverse of a matrix has not been cached 
#under the parent function, it will compute and cache it.   


#The makeCacheMatrix function allows for a matrix to be set and stored and for the inverse of the matrix to be set and stored
#The function contains the following 4 functions: setting of a square matrix x "set"; 
#getting matrix x "get"; setting the inverse of matrix x "setinverse", and getting the inverse of matrix x "getinverse"

makeCacheMatrix <- function(x=matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function()x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function()i
     list(set=set, get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}

#The cacheSolve function takes as an argument the matrix defined under makeCacheMatrix 
#(e.g. n <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2)) then call cacheSolve(n)).
#The function will search for the inverse (i) matrix cached with the makeCacheMatrix function that matches the argument of cacheSolve. 
#If it matches the argument called by cacheSolve, then the message "getting cached data" will be returned along with the inverse matrix 
#(i.e. the inverse matrix will not be re-computed). If there is no cached match, then the inverse matrix will be computed and cached as 
#x$setinverse under the makeCacheMatrix function.  

cacheSolve <- function(x,...){
     i <- x$getinverse()
     if(!is.null(i)){
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i
}

