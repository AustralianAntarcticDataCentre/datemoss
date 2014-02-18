#' Extract growth rate estimates
#'
#' @param segments dataframe: the segment data, as returned by read_segment_data()
#' @param s dataframe: fitted date estimates, as returned by fit_dates()
#'
#' @return A dataframe of growth rate estimates (in units of mm/year)
#' @export
get_growth_rate=function(s,segments) {
    ## for mcmc, we can get an estimate of growth rate for every MCMC sample (row in s), therefore distributions on the estimates of growth rate
    #fhat=as.data.frame(t(apply(s,2,function(x) c(mn=mean(x),sd=sd(x),quantile(x,c(0.5,0.025,0.975))))))
    s.growthrate=-t(apply(t(diff(t(s))),1,function(z){ segments$segment_length_mm/z }))
    ## or, growth rate based on mean estimates of the segment boundary dates
    #fhat.growthrate=-segments$length/diff(fhat$mn)
    #segments$fitted.growthrate=fhat.growthrate
    ## or could use colMeans(s.growthrate), but this is strongly skewed by outliers (needs thought)
    ## maybe use the mode instead:
    ## with library(modeest) apply(s.growthrate,2,function(z){mlv(z,method='shorth')[[1]]})
    ## or directly from density: apply(s.growthrate,2,function(z){d=density(z,from=0,to=10); d$x[which.max(d$y)] })
    s.growthrate
}
