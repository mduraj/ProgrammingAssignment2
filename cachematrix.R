## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv<-matrix()   ## expected result - inverted x
set<-function(g) {
        x<<-g
        inv<<-matrix()
}       ## sets an input matrix
        
get<-function() {x} ## it returns our matrix x
setinv<-function(y) {inv<<-y} ## sets inv as y
getinv<- function() {inv} ## returns a result
list(set=set,get=get,setinv=setinv,getinv=getinv) ##prints functions defined inside


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       invv<-x$getinv()  ## named invv to distinguish invv and inv from previous function
       if(!is.na(invv[1,1])) {  ## checks if result is either a numeric matrix or 1x1 matrix NA
               message("getting cached data") ## if so it prints this message
               return(invv) ## and returns inversed matrix
       }
       data<-x$get() ## if not we need to load our matrix
       invv<-solve(data) ## and calculate its inverse...
       x$setinv(invv) ## ...set inverse matrix also...
       invv ##...and print this inverse
}
