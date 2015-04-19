## Put comments here that give an overall description of what your
## functions do
## These 2 functions are written same manner as makeVector and cachemean functions are

## Write a short comment describing this function
##makeCacheMatrix creates a square matrix (we want to invert that matrice with solve())
##with cacheSolve. The matrix elements are filled with 4 functions

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function()x
        
        setreverse<-function(solve)m<<-solve
        
        getreverse<-function()m
        
        fcts<-list(set=set,get=get,setreverse=setreverse,getreverse=getreverse)
        ##We need a matrix as result
        fcts<-matrix(fcts,byrow=TRUE,4,4)
}


## Write a short comment describing this function
## cacheSolve takes makeCacheMatrix matrix as argument : if a matrix is cached, then
## a matrix has been reversed, and we can gets it from the cache. Else, a reverse is
## calculated and set into the cache
cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x[[4]]() ##function getreverse is used to get the cached matrix
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x[[2]]() ##function get to take a new matrix
        m<-solve(data,...)
        x[[3]](m) ##function setreverse to put the result in the cache
        m              
}
