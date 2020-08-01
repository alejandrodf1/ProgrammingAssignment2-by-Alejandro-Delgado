
## The function recieve a matrix and we solve the inverse. The computacional work then

makeCacheMatrix <- function(x=matrix()){
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

## we return the inverse matrix with this last function
cacheSolve <- function(x, ...){
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
