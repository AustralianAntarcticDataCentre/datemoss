## construct a function that, given segment boundaries (dates), calculates the PMC we would expect to see from a measurement of that sample
## this function for internal package use, so not exported

seg_c14fun=function(cal) {
    ##function(fhat) { sapply(1:(length(fhat)-1),function(z){ integrate(f=atm_c14,lower=fhat[z],upper=fhat[z+1])$value/(fhat[z+1]-fhat[z]) }) }
    ## faster to use cumulative pmc, avoids integral at each step
    function(fhat) { sapply(1:(length(fhat)-1),function(z){ diff(cal$atm_cumc14(fhat[z:(z+1)]))/(fhat[z+1]-fhat[z]) }) }
}
