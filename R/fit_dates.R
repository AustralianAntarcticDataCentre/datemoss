#' Estimate dates
#'
#' Uses an MCMC method to estimate date span of each segment, given the measured d14C for each segment and the atmospheric d14C curve
#'
#' @param segments dataframe: the segment data, as returned by read_segment_data()
#' @param f0 numeric: vector of initial date estimates, as returned by estimate_initial()
#' @param cal dataframe: the calibration data, as returned by read_calibration()
#' @param mcmc.thin numeric: thinning parameter for MCMC fitting
#' @param mcmc.iters numeric: number of iterations for MCMC fitting
#' @param mcmc.burnin numeric: number of burn-in iterations for MCMC fitting
#' @param mcmc.sigma numeric: sigma parameter for MCMC fitting
#'
#' @return A vector of date estimates (specified as fractional years)
#' @export

fit_dates=function(segments,f0,cal,mcmc.thin=25,mcmc.iters=2000,mcmc.burnin=200,mcmc.sigma=4) {

    mcmc <- function(seg_c14,ys,ts,sigma,tmin=1940,iters=1000,thin=10) {
        n <- length(ts)
        s <- matrix(0,iters,n)
        ## Residuals, and contribution to log posterior from each segment
        rs <- ys-seg_c14(ts)##diff(cumcal(ts))
        logps <- dnorm(rs,0,sigma,log=T)
        for(k1 in 1:iters) {
            for(k2 in 1:thin) {
                ## Red-black update - we update the times in two interleaved sets
                for(rb in 1:2) {
                    ## Indices to update
                    is <- seq.int(rb,n-1,by=2)
                    ## New proposal
                    lwr <- c(tmin,ts)[is]
                    upr <- ts[is+1]
                    ts.new <- ts
                    ts.new[is] <- runif(length(is),lwr,upr)
                    ## Contribution to log posterior from each segment of proposal
                    rs.new <- ys-seg_c14(ts.new)##diff(cumcal(ts.new))
                    logps.new <- dnorm(rs.new,0,sigma,log=T)
                    ## Metropolis-Hastings rule - which proposals are kept?
                    logp.is <- c(0,logps)[is]+logps[is]
                    logp.is.new <- c(0,logps.new)[is]+logps.new[is]
                    keep <- logp.is.new-logp.is > log(runif(length(is)))
                    is <- is[keep]
                    ts[is] <- ts.new[is]
                    ## Contribution to log posterior from updated segments
                    rs <- ys-seg_c14(ts)##diff(cumcal(ts))
                    logps <- dnorm(rs,0,sigma,log=T)
                }
            }
            s[k1,] <- ts
        }
        s
    }

    ## some things
    seg_c14=seg_c14fun(cal)
    fit.tmin=attr(f0,"tmin")

    ## Run the chain
    s=mcmc(seg_c14,rev(segments$pmc),rev(f0),mcmc.sigma,tmin=fit.tmin,iters=mcmc.iters+mcmc.burnin,thin=mcmc.thin)
    ## s is arranged with each row containing a solution, with the most recent date in the last column and the earliest date in the first column. We want the column ordering reversed
    s=s[,ncol(s):1]
    ## and discard the burn-in samples
    s=s[(mcmc.burnin+1):nrow(s),]

    s
}
