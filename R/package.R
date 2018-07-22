#' A simple implementation of Binary Indexed Tree as an R6 class.
#'
#' Methods: \itemize{
#' \item \code{new(init)} Initializes from a vector init.
#' \item \code{update(location, new.val)} Replaces the item at location with new.val.
#' \item \code{query(l, r)} Returns the sum of the interval [l, r].
#' \item \code{show.BIT ()} Returns the Binary Indexed Tree.
#' \item \code{show.origin()} Returns the vector.
#' }
#'
#' @export
#' @importFrom R6 R6Class
#' @examples
#' tmp <- BinaryIndexedTree$new(c(2,3,2,5,1))
#' tmp$update(1,3)
#' tmp$query(1,5)
#' tmp$show.origin()
#' tmp$show.BIT()
  
BinaryIndexedTree <- R6Class("BinaryIndexedTree",
                             
                            public = list(
                              initialize = function(init){
                                
                                private$len <- length(init)
                                
                                for (i in 1:private$len)
                                {
                                  private$ORI[i] = init[i]
                                  private$BIT[i+1] <- init[i]
                                  j = i-2
                                  while (j>=i-private$lowbit(i)){
                                    print(private$BIT[i+1]+init[j+1])
                                    private$BIT[i+1] <- private$BIT[i+1]+init[j+1]
                                    j = j-1
                                  }
                                }
                                
                              },
                              update = function(loc, new.val){
                                
                                j = loc
                                while (j <= private$len){
                                  private$BIT[j+1] <- private$func(private$BIT[j+1], new.val)
                                  j = j + private$lowbit(j)
                                }
                                
                              },
                              query = function(l, r){
                                
                                ans1 = private$init.val
                                i = l-1
                                while (i)
                                {
                                  ans1 = private$func(ans1, private$BIT[i+1])
                                  i = i - private$lowbit(i)
                                }
                                
                                ans2 = private$init.val
                                i = r
                                while (i)
                                {
                                  ans2 = private$func(ans2, private$BIT[i+1])
                                  i = i - private$lowbit(i)
                                }

                                return(ans2-ans1)
                              },
                              show.BIT = function(){
                                return(private$BIT)
                              },
                              show.origin = function(){
                                return(private$ORI)
                              }
                              
                            ),
                            
                            private = list(
                              BIT = c(0),
                              ORI = c(0),
                              func = function(x, y){ return(x+y) },
                              len = 0,
                              init.val = 0,
                              lowbit = function(x){
                                return(bitwAnd(x, -x))
                              }
                            )
                            
                            )
