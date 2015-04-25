####################################################################################################
##Assignment 2 : Caching the inverse of a matrix
##Functions makeCacheMatrix and cacheSolve
##
##- makeCacheMatrix: creates a special "matrix" object that can cache its inverse
##- cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.
##
##These 2 functions are written on basis makevector and cachemean functions examples.
####################################################################################################



################################################################################
## makeCacheMatrix(x=matrix())
## Creates a square matrix (we want to invert that matrice with solve()) in the 
## cacheSolve function. The matrix elements are filled with 4 functions
################################################################################

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function()x
        
        setreverse<-function(solve)m<<-solve
        
        getreverse<-function()m
        
        ##Here a list that contains all our functions
        fcts<-list(set=set,get=get,setreverse=setreverse,getreverse=getreverse)
        ##We need a matrix as result
        fcts<-matrix(fcts,byrow=TRUE,4,4)
}



################################################################################
## cacheSolve(x,...)
## Takes makeCacheMatrix matrix as argument : if a matrix is cached, then a 
## matrix ha been reversed, and we can get it from the cache. Else, a reverse
## is calculated and set into the cache.
################################################################################

cacheSolve <- function(x,...) {
        ## Returns a matrix that is the inverse of 'x'
        m<-x[[4]]() ##function getreverse as element 4 is used to get the cached matrix
        if(!is.null(m)){
                ## A reversed matrix is in cache
                message("getting cached data")
                return(m)
        }
        data<-x[[2]]() ##function get in element 2 is used to take a new matrix
        m<-solve(data,...) ##function solve should inverse the matrix
        x[[3]](m) ##function setreverse, element 3, is used to store the result in the cache
        m              
}
