## Function makeCacheMatrix initializes m in the parent environment
##     and defines 3 nested functions:  
## set-takes y as an argument and assigns it to x in the parent environment, and
##     assigns null to m in the parent environment(clearing any previously cached value);
## get-which retrieves x from the parent environment; 
## setinvert-which assigns the inverted matrix to m in the parent environment;
## getinvert-which retrieves m from the parent environment (e.g. the inverted matrix)
##     and then returns a list with the 4 functions, x and m, as elements 
##
## The second function, cacheSolve, retrieves the inverted matrix from an object of type 'makeCacheMatrix'.
##      if the value of the retrieved matrix is NOT null, then the function will return the inverted matrix
##      in m (e.g. a valid, cached, inverted matrix)...otherwise, if the value IS null, then the function 
##      will get the initial matrix from the input object; calculate the inverted matrix; set the inverted 
##      matrix within the object, and return m, the inverted matrix.


makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL                             # initializes m in the parent environment
      
        set<-function(y=matrix) {           # accepts y, the initial, inputted matrix
                x<<-y                       # assigns the y, the initial inputted matrix to x in the parent environment
                m<<-NULL                    # initalizes m in the parent environment (clears any previously cached matrix)
               
        }
        get<-function() x                   # retrieves x, the inputted matrix, from the parent environment
        setinvert<-function(invert=matrix) m<<-invert       #assigns the inverted matrix to m in the parent environment
        getinvert<-function() m             # retrieves m from the parent environment (e.g. the inverted matrix)
        
        list(set=set, get=get,              # defines a list of the 4 functions above, x and m, to be returned
             setinvert=setinvert,
             getinvert=getinvert)
}

## Second function to retrieve the stored inverted matrix, if one exists, or to calculate the inverted matrix and return
## that value to the parent environment, in m.

cacheSolve <- function(x, ...) {            # defines the cacheSolve function with an object of type: makeCacheMatrix as an argument
        m<-x$getinvert()                    # attempt to retrieve an inverted matrix from the input object
        
        if(!is.null(m)) {                   # if a value exists in m, other than null, then 
                message("getting cached data")   # issue a message stating the cached inverted matrix is being retrieved, and, 
                return(m)                   # return the matrix
        }
        data<-x$get()                       # if the value in m is null, then get the initial matrix from the object
        m<-solve(data)                      # calculate the inverted matrix, and store in m
        x$setinvert(m)                      # set the inverted matrix into the input object
        m                                   # and return the inverted matrix
        
        ## Return a matrix that is the inverse of 'x'
}
