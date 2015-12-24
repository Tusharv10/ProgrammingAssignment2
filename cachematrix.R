## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) {
        z<- NULL
        set<- function(b){
                a<<-b
                z<<- NULL
}
        get<- function() a
        setinverse<- function(inverse) z<<- inverse
        getinverse<- function() z
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
}
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
## Return a matrix that is the inverse of 'x'
}
