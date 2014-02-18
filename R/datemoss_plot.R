#' Helper plotting routines
#'
#' @param segments dataframe: the segment data, as returned by read_segment_data()
#' @param f0 numeric: vector of initial date estimates as returned by estimate_initial()
#' @param cal dataframe: the calibration data, as returned by read_calibration()
#' @param s dataframe: fitted date estimates, as returned by fit_dates()
#' @param growthrate dataframe: fitted growth rate estimates, as returned by get_growth_rate()

#' @rdname datemoss_plot
#' @export
plot_initial=function(cal,segments,f0) {
    acquisition_date=attr(segments,"acquisition_date")
    with(cal$x14c,plot(Year,PMCsmoothed,type="b",xlim=c(1950,2013))) ## calibration curve
    with(segments,points(date_mean,pmc,pch=2,col=2)) ## mean oxcal dates
    temp=c(acquisition_date,segments$date_mean+c(diff(segments$date_mean)/2,NA)) ## segment end-points for oxcal solution
    temp[length(temp)]=2*temp[length(temp)-1]-temp[length(temp)-2]
    seg_c14=seg_c14fun(cal)
    points(segments$date_mean,seg_c14(temp),pch=4,col=6) ## measured c14 implied by oxcal solution
    temp=sapply(1:(length(temp)-1),function(z){ lines(temp[z:(z+1)],seg_c14(temp)[z]*c(1,1),col=6) }) ## segment extents for oxcal solution
    temp=f0[1:(length(f0)-1)]+diff(f0)/2 ## date of segment midpoint for initial solution
    points(temp,seg_c14(f0),pch=4,col=4)
    temp=sapply(1:(length(f0)-1),function(z){ lines(f0[z:(z+1)],seg_c14(f0)[z]*c(1,1),col=4) })
    legend("topright",legend=c('Atmospheric PMC curve','Measured PMC/Oxcal dates','Segment PMC using Oxcal dates','Segment PMC using initial dates'),col=c(1,2,6,4),pch=c(1,2,4,4))
}

#' @rdname datemoss_plot
#' @export
plot_date_estimates=function(s) {
    ## plot the MCMC estimates of the dates of each segment boundary
    plot(x=NA,type="n",xlim=range(s),ylim=c(1,ncol(s)+1),ylab="Segment",xlab="Year")
    for (k in 2:ncol(s)) { thisd=density(s[,k]); lines(thisd$x,thisd$y/max(thisd$y)+k,xlab="",main="",col=k) }
}

#' @rdname datemoss_plot
#' @export
plot_fit_summary=function(segments,cal) {
    opar=par()
    par(mfrow=c(2,1))
    with(segments,plot(depth_mm,pmc,type="p",ylim=range(segments[,c('fitted.c14','pmc')])))
    with(segments,points(depth_mm,fitted.c14,col=4,pch=2))
    legend("topleft",legend=c("Measured","Fitted"),pch=c(1,2),col=c(1,4))
    with(cal$x14c,plot(Year,PMCsmoothed,type="b",xlim=c(1950,2013)))
    with(segments,points(fitted.date_segment_mid,pmc,pch=2,col=2))
    with(segments,points(fitted.date_segment_mid,fitted.c14,pch=4,col=4))
    temp=sapply(1:nrow(segments),function(z){ lines(segments[z,c('fitted.date_segment_start','fitted.date_segment_end')],segments$fitted.c14[z]*c(1,1),col=4) })
    legend("topright",legend=c('Atmospheric PMC curve','Measured PMC','Segment PMC from fitted dates'),col=c(1,2,4),pch=c(1,2,4))
    ##with(segments,plot(fitted.date_segment_mid,fitted.growthrate,type="p",pch=3,xlab="Segment midpoint date",ylab="Growth rate (mm/year)",xlim=c(1950,2013)))
    par(opar)

}


#' @rdname datemoss_plot
#' @export
plot_growth_rate=function(growthrate,s) {
    fhat=as.data.frame(t(apply(s,2,function(x) c(mn=mean(x),sd=sd(x),quantile(x,c(0.5,0.025,0.975))))))
    fhat.mid_date=fhat$mn[1:(length(fhat$mn)-1)]+diff(fhat$mn)/2 ## mean segment date (i.e. date of segment midpoint)

    plot(x=NA,type="n",xlim=range(-0.5,10),ylim=range(fhat.mid_date)+c(0,2),ylab="Segment",xlab="Growth rate (mm/year)")
    for (k in 1:ncol(growthrate)) { thisd=density(growthrate[,k],from=0, to=10); lines(thisd$x,thisd$y/max(thisd$y)*2+fhat.mid_date[k],xlab="",main="",col=k) }
}
