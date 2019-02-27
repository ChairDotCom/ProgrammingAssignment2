## This script creates a list of functions applied to the matrix "x" 
## and calculates the inverse of the matrix "x". If matrix "x" hasn't been
## changed then previously calculated cached inverse would be returned


## Create matrix "x" with list of functions that could be applied to it

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(m){
                x<<-m
                inv<<-NULL
        }
        get<-function() x
        set_inv<-function(inverse) inv<<-inverse
        get_inv<-function() inv
        list(set=set, get=get, set_inverse=set_inv, get_inverse=get_inv)
}


## Calculate inverse of the matrix "x". In case inverse was calculated before
## and matrix "x" hasn't been changed since, cached value of matrix inverse 
## will be returned

cacheSolve <- function(x, ...) {
        inv<-x$get_inv()
        if (!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$set_inv(inv)
        inv
}
