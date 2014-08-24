## These functions are for the peer assigment of the R Programming course from coursera,which is really cool.
## Two functions are contained:



## the first one is makeCacheMatrix(),which creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
	      x<<-y
	      m<<-NULL
      }
      get<-function()x
      setinverse<-function(inverse)m<<-inverse
      getinverse<-function()m
      list(set=set,get=get,setinverse=setinverse,
           getinverse=getinverse)
}


## the second one is cacheSolve(),which calculate the inversed matrix from which the first function send back.
## If the inversed matrix has already been process,then it will get the inversed matrix from the cache and skip 
## the processing of this function.Otherwise, it calculate the inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
        if(!is.null(m)) {
                message("getting inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
