## functions to cache the inverse of a matrix in order to
## save computation time

## makeCacheMatrix creates a special vector which stores
## the original value and the cached inverse value of matrix
x<-NULL
y<-NULL
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL 
        }
        get<-function()x
        setinverse<-function(inverse)inv<<-inverse
        getinverse<-function()inv
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## cacheSolve calculate the inverse of the special vector created
## by makeCacheMatrix

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
