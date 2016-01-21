## With this functions, u can create a special "matrix-object", that can cache it's inverse and u can print
##the reverse or use it. This keeps time when u invert complex matrixes and wanna use this later on.

## the x<-makeCacheMatrix() funktion creates a special matrix-object "x". U have to give an matrix as an argument like 
##this: m <- matrix(c(-1, -2, 1, 1), 2,2)
##      x <- makeCacheMatrix(m)
## the object "x" is containing the lists the functions and u can bring them up by using x$getinvert() as 
## example. The functions store the informations about the matrix which are:
## set: set the matrix
## get: get the matrix (to see, what matrix is used, u can type x$get())
## setinvert: sets the inverted-matrix by using solve() on the matrix 
## getinvert: gets the inverted-matrix (to see, whats the inverted-matrix, u can type x$getinvert(), if the
##              inverted-matrix has been compueted earlier. 
## these are the parts of the first function. The object u create can now be used to store these information.

makeCacheMatrix <- function(x){
        
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function(){x}
        setinvert<-function(solve){m<<-solve(x)}
        getinvert<-function(){
                m
        }
        list(set=set,get=get,setinvert=setinvert,getinvert=getinvert)
}


## This function uses the Information stored through makeCacheMatrix to get the inverted-matrix. there are 2 
## ways: if the inverted-matrix is allready computed (m!=NULL), it prints the inverted-matrix and shows that
## it was allready computed with a message ("getting cached Data")
## in the object created with "makeCacheMatrix".
## if the inverted-matrix hasn't been computed (m=NULL), it will be computed, printed and stored in the
## special object created with "makeCacheMatrix". I hope u understand everything :) Have a nice day

cacheSolve<-function(x, ...){
        m<- x$getinvert()
        if(!is.null(m)){
                message("getting cached Data")
                return(m)
        }
        data <- x$get()
        m <- solve(a=data, ...)
        x$setinvert(m)
        m
}

##pss: u can try the function like this:
##      m <- matrix(c(-1, -2, 1, 1), 2,2)
##      x <- makeCacheMatrix(m)
##      cacheSolve(x)
##      cacheSolve(x)

