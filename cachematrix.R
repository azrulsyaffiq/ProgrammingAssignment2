## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##Initialize the inverse property
    i<-NULL
    
    ##Method to set the matrix
    set<-function(matrix){
        m<<-matrix
        i<<-NULL
    }
    
    ##Method to get the matrix
    get<-function(){
        ##Return the matrix
        m
    }
    
    ##Method to set the inverse of the matrix
    setinverse<-function(matrix){
        i<<-inverse
    }
    
    ##Method to get the inverse of the matrix
    getinverse<-function(){
        ##Return the inverse property
        i
    }
    
    ##Return a list of the methods
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m<-x$getinverse()
    
    ##Return inverse if its already set
    if(!is.null(m)){
        message("getting cached data")
        return(m)

    }
    
    ##Get the matrix from input above
    data<-x$get()
    
    ##Calculate the inverse using matrix multiplication
    m<-solve(data)%*% data
    
    ##Set the inverse to the input
    x$setinverse(m)
    
    ##Return the matrix
    m
}
