## There are two functions here. First is makeCacheMatrix and second is cacheSolve. The first one takes an invertible matrix as its argument
## and returns a list of four functions. Then the second function cachesolve is there which takes this list as an argument and returns 
## the inverse of orignal matrix.

## makeCacheMatrix takes an invertible matrix as its argument and returns a list containing four functoons. 
## Here set() function substitutes the value of input matrix with its argument(b) globally. get 
## is used to returns back the matrix . Similarly setinverse() substitutes the value of z with
## its argument(inverse). getinverse() returns the value of z. 

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
## Returns a list of functions
}


## Now the list makeCacheMatrix returns is fed into cacheSolve as its argument. It first checks if the inverse of this matrix is already
## cached in the memory or not. If yes, it returns that cached value and nothing else is executed. If no, it finds the inverse using solve()
## and store it into cache so that in future if we require the inverse of the same matrix, we can simply get cached data.

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
