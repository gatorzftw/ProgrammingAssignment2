## Caching the inverse of a matrix allows one to save time by not repeatedly calculating it. 
## The two functions below, makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix. 

## makeCacheMatrix creates a list of a function that first sets the value of a matrix m, 
## gets the values of the matrix, set the value of the inverse of that matrix m,  
##and then lastly gets values for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m <<- inverse
  getinverse<-function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of a matrix, calling it inv. 
## The function first verifies that the function has not already been computed
## If it has, it gets the results from the stored responses and skips the calculation. If not,
## it computes the inverse, calling it inv, and sets the values in the cache using 
## setinverse.
cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    }  
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv  
}
