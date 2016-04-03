#this pair of functions compute the inverse of a matrix 
#in a computationally-efficient way. If the contents of a matrix are not 
#changing these are cached with makeCacheMatrix and returned with cachesolve

#makeCacheMatrix is function that takes as argument a matrix x and
#returns a list of four elements: 
#set (sets the value of the matrix), 
#get (gets the value of the matrix),
#setinverse (sets the inverse of the matrix)
#getinverse (gets the inverse of the matrix).

makeCacheMatrix <-function(x=matrix()){
        i<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get <-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse <-function() i
        list(set=set, get = get, setinverse=setinverse, getinverse=getinverse)
}


#chachesolve calculates the inverse of the matrix returned by makeCacheMatrix.
#if the inverse has been calculated before (ie i is not null) the function 
#returns the chached value. If the matrix x has changed the inverse is calculated
#and the new value is set with the setinverse function

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
