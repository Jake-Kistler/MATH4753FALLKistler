#' mySample draws 'n' values with replacement from '1:10' and plots the relative frequency as a barplot.
#' Repeats this 'iter' times with pause between sample polling
#'
#' @param n integer. Sample size for each iteration
#' @param iter integer. Number of iterations (or the number of plots to produce)
#' @param time numeric. Seconds to pause between sample plots
#'
#' @returns 'NULL' due to divsion error
#' @export
#'
#' @examples
#' mysample(n = 20, iter = 1, time = 1)
mysample=function(n, iter=10,time=0.5)
{
  for( i in 1:iter)
  {
    #make a sample
    s=sample(1:10,n,replace=TRUE)

    # turn the sample into a factor
    sf=factor(s,levels=1:10)

    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
