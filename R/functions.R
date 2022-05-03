

#' @title my read function
#'
#' @description This function reads .csv data into the environment
#'
#' @param dird directory csv is stored in
#' @param csv the title of the .csv title as a string
#'
#' @import utils
#'
#' @return table containing data
#' @export
#'
#' @examples
#' \dontrun{dird = "C:\\Users\\Admin\\Desktop\\Applied Statistical Methods\\Lab 4\\"
#' data = myread(dird, 'SPRUCE.csv')}
#'
myread=function(dird, csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}





#' @title my binomial function
#' @description finds the number of successes in each iteration given a probability.
#' The function adds the number of iterations that received each number of successes,
#' divides by overall number of iterations, and represents the distribution as a percentage.
#' @param iter number of iterations
#' @param n number of samples taken each iteration, with replacement
#' @param p probability of TRUE sample
#'
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @return a bar graph showing the distribution and associated table
#' @export
#'
#' @examples mybin()
#'
#'
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1),
          main=paste("binomial distribution  \n", " iteration ",
                     i, " n= ", n,sep=""),
          xlab="Number of successes"
  )
  succ.tab/iter
}




#' @title my multinomial function
#' @description finds the number of successes in each iteration given an array of probabilities, 1 per option.
#' The function adds the number of iterations that received each number of successes,
#' divides by overall number of iterations, and represents the distribution as a percentage.
#' @param iter number of iterations
#' @param n number of samples taken each iteration, with replacement
#' @param p array of probabilities for each outcome, add to 1
#'
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @return a barplot showing the relative frequencies and a corresponding table
#' @export
#'
#' @examples mymult()
#'
mymult=function(iter=100,n=10, p=c(1,1,1,1)/4){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #The number of categories is k
  k=length(p)
  # Make a matrix that will hold the frequencies in each sample
  tab.mat=matrix(NA,nrow=k,ncol=iter, byrow=TRUE)


  for(i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(1:k,n,replace=TRUE, prob=p)
    #Collect all the frequencies of each of the k values
    tab.mat[,i]=table(factor(sam.mat[,i],levels=1:k))
  }
  # sum the frequencies
  freq=apply(tab.mat,1,sum)
  # put names to them
  names(freq)=1:k
  #create a barplot of relative freq
  barplot(freq/(n*iter),col=rainbow(k),
          main=paste("multinomial distribution  \n", " iteration ",
                     i, " n= ", n,sep="")
  )
  tab.mat
}





#' @title my hypergeometric function
#' @description a function which shows the hypergeometric distribution
#' of a sample. Shows probability of number of successes while sampling without replacement
#'
#' @param iter number of iterations
#' @param N population size
#' @param r number of successes in population
#' @param n number of samples each iteration without replacement
#'
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @return a bar plot of the distribution and associated table
#' @export
#'
#' @examples myhyper()
#'
myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1),
          main=paste("hypergeom. distribution  \n", " iteration ", i, " n= ", n,sep=""),
          xlab="successes"
  )
  succ.tab/iter
}





#' @title my sample function
#' @description takes n samples each iteration with replacement and
#' shows the resultant distribution
#'
#' @param n number of samples each iteration
#' @param N size of each sample
#' @param rep with or without replacement
#' @param iter number of iterations
#' @param time time between showing each plot
#' @param last indicates that only last bar plot should be displayed
#'
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @return a barplot of the distribution and a corresponding table
#' @export
#'
#' @examples mysample(5)
#'
mysample=function(n, N=10, iter=10,time=0.1, last=FALSE, rep=TRUE){
  for( i in 1:iter){
    #make a sample
    s=sample(1:N,n,replace=rep)
    # turn the sample into a factor
    sf=factor(s,levels=1:N)
    #make a barplot
    if(last == FALSE) {
      barplot(table(sf)/n,beside=TRUE,col=rainbow(N),
              main=paste("sample \n ", " iteration ", i, " n= ", n,sep="") ,
              ylim=c(0,0.2)
      )
    } else {
      if(i == iter)
        barplot(table(sf)/n,beside=TRUE,col=rainbow(N),
                main=paste("sample  \n", " iteration ", i, " n= ", n,sep="") ,
                ylim=c(0,0.2)
        )
    }
    #release the table
    Sys.sleep(time)
  }
  table(sf)
}


#' @title my probability curve function
#' @description uses mean and sd to create normal distribution, calculates
#' probability below a and shows as shaded region and prints value to console
#'
#' @param mu average of distribution
#' @param sigma sd of distribution
#' @param a upper limit for probability, filled region
#'
#' @import grDevices
#' @import graphics
#' @import utils
#' @importFrom stats dnorm pnorm
#'
#' @return plot of norm distribution w/ filled region, probability as list
#' @export
#'
#' @examples
#' myncurve(5, 2, 3)
#'
myncurve = function(mu, sigma, a){
  x=seq(mu-3*sigma, mu+3*sigma,length.out = 1000)
  curve(dnorm(x,mean=mu,sd=sigma),
        xlim = c(mu-3*sigma, mu + 3*sigma),
        ylab=paste('norm  mean:', mu,'  sd:', sigma))
  xcurve=seq(mu-3*sigma, a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  cat('Prob:',round(pnorm(a, mean= mu, sd= sigma), 4))
}



#' @title my ddt function for Project 1
#' @description subsets the ddt df by a species given as a parameter, generates
#' a corresponding plot, df before and after subsetting, and a relative frequency
#' of river table
#'
#' @param df the ddt dataframe or one similar
#' @param spec the species selected by the user
#'
#' @import dplyr
#' @import ggplot2
#' @import grDevices
#' @import graphics
#' @import utils
#'
#' @return a named list with a plot of the subsetted df, df before and after
#' subsetting, and a relative frequency of river table
#'
#' @export
#'
#'@examples
#'\dontrun{
#'myddt(df = ddt, spec  = "CCATFISH")}
#'
myddt <- function(df, spec){
  # use dplyr to select only given species
  df %>%
    filter(.data$SPECIES == {{spec}}) -> dat

  # write a csv to the working directory with subsetted ddt df
  write.csv(dat, paste('LvsWfor',spec,'.csv', sep=''))

  # create a plot of Length vs. Weight with subsetted data
  # point color corresponds to river and a quadratic curve is displayed
  plot = ggplot(dat, aes(x = .data$WEIGHT, y = .data$LENGTH)) +
    geom_point(aes(color = .data$RIVER)) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
    labs(title="Nick Castle",
         subtitle=paste("Length vs Weight of", spec),
         caption="Source: DDT.csv",
         x="Weight",
         y="Length")

  # the function returns a list with the plot, df, subsetted df, and a relative frequency of river table
  list('plot' = plot,
       'dataframe' = df,
       'subsetted dataframe' = dat,
       'relative frequency of river table' = table(df$RIVER)/length(df$RIVER))
}




#' @title central limit theorem applied to poisson distribution
#' @description applies the central limit theorem to the poisson distribution
#' and displays a histogram of the means distribution density
#'
#' @param n sample size
#' @param iter number of iterations
#' @param lambda mean and sd of the poisson distribution
#' @param ... other parameters of the main histogram
#'
#' @return returns a list of the means of each iteration and plots of probability density
#' for the mean, the poisson distribution, and a relative frequency plot
#' @export
#'
#' @examples
#' mycltp(5, 100)
#'
mycltp=function(n,iter,lambda=10,...){

  y=rpois(n*iter,lambda=lambda) # random sample from poisson distribution
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # place samples in matrix by column
  w=apply(data,2,mean) # take mean of samples by column
  param=hist(w,plot=FALSE) # find the max density with empty plot
  ymax=1.1*max(param$density) # make max bound with a lil wiggle room

  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE)) # layout matrix for multiple graphs

  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," \nlambda=",lambda,sep=""),
       xlab="Sample mean",...) # the primary hist is made
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

  # Now make a new barplot because y is discrete
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" ) # relative frequency plot
  x=0:max(y) # x values for plot
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y") # probability function plot
}




#' @title Bootstrapping function for basic stats
#' @description This function uses bootstrapping to apply a function to a sample
#'
#' @param iter number of iterations
#' @param x vector to evaluate
#' @param fun function to apply
#' @param alpha confidence value
#' @param cx numeric character expansion factor
#' @param ... other params for hist
#'
#' @return a list of confidence interval, function, vector, and results per iter
#' @export
#'
#' @examples
#' x = 1:10
#' myboot2(x=x)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   # calculate sample size

  y=sample(x,n*iter,replace=TRUE) # retrieve samples for all its
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # place in matrix by row
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# find the requested CI
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap\nsample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...) # create the histogram

  # make a matrix of source data in a collumn for other stats
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  pte=apply(mat,2,fun) # find point estimate of whole column
  abline(v=pte,lwd=3,col="Black") # Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      # Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x,xstat=xstat))# output
}



#' @title my ml for normal distribution
#' @description this uses maximum likelihood to determine the mean of a sample
#'
#' @param x data to evaluate
#' @param mu vector of potential mean values
#' @param sig vector of potential sd values
#' @param ... extra parameters for the plot
#'
#' @return data, maximum likelihood, coords
#' @export
#'
#' @examples
#' mymlnorm(x=c(10,12,13,15,12,11,10),mu=seq(10,14,length=1000),
#' sig=seq(0.1,4,length=1000),lwd=2,labcex=1)
#'
mymlnorm=function(x,mu,sig,...){  #x sample vector
  nmu=length(mu) # number of values in mu
  nsig=length(sig)
  n=length(x) # sample size
  zz=c()    # get zz readyr
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))   # log like fn for normal dist
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j]) # create the matrix
    # col 1 of z contains lfun evaluated at each x with first value of mu,
    # col2 each x with 2nd value of m
    y=apply(z,2,sum)
    # sum for all mus
    zz=cbind(zz,y)
    ## all log l values
  }
  maxl=max(exp(zz)) # find maxes index and retrieve value
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),...) # create contours
  mlx=round(mean(x),2)  # theoretical
  mly=round(sqrt((n-1)/n)*sd(x),2)
  #axis(1,at=c(0:20,mlx),labels=sort(c(0:20,mlx)))
  #axis(2,at=c(0:20,mly),labels=TRUE)
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  # Now find the estimates from the co-ords
  muest=mu[coord[1]]
  sigest=sig[coord[2]]
  abline(v=muest, h=sigest) # crosshair
  return(list(x=x,coord=coord,maxl=maxl))
}

