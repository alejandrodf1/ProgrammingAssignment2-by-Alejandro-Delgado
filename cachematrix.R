## Put comments here that give an overall description of what your
## functions do

## In this function we store a matrix and its inverse then

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        put<- function(y){
                x<<- y
                z<<-NULL
        }
        take<- function() x
        putinverse<-function(inverse) z <<- inverse
        takeinverse<- function() z
        list(put = put,
             take=take,
             putinverse=putinverse,
             takeinverse=takeinverse)
}


## Here we return the value or the matrix that was solve before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                z <- x$takeinverse()
        if(!is.null(z)){
                message("getting cached data")
                return(z)
        }
        data<-x$take()
        z<- solve(data, ...)
        x$putinverse(z)
        z
}
