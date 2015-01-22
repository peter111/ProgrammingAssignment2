## This R file contains two functions: makeCacheMatrix
## and cacheSolve that are used to create a special object
## that stores a matrix and cache's its inverse matrix


## makeCacheMatrix creates a list of functions to set and 
## obtain the value of matrix and their inverse value

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve checks if there is value for above created object. 
## If yes, it obtains the inverse matrix from cache. If not,
## it computes the inverse matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(is.matrix(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
