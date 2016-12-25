## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It takes a matrix as input and stores it's data and inverse value. 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inversem) inv<<-inversem
        getinverse<-function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}


## Write a short comment describing this function
## This function takes a list as an input. The list is then used to get data for inverse value
## if the value of inverse already exists (i.e. it is not NULL), then the inverse is retrieved
## from the cachematrix and returned directly. Else, only the matrix data is retrieved. Then
## inverse is calculated and stored in cache using setinverse function
cacheSolve <- function(x, ...) {
        inv<-x$getinverse() 
        if(!is.null(inv)) {   
                print("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
