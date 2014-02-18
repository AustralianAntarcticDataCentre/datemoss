#' Initialize date estimates
#'
#' @param segments dataframe: the segment data, as returned by read_segment_data
#' @param tmin numeric: the earliest allowable date (specify as a fractional year)
#' @param method string: method to use for generating the initial estimates. Methods other than "random" (the default) are experimental
#'
#' @return A vector of date estimates (specified as fractional years)
#' @export

estimate_initial=function(segments,tmin,method="random") {
    ## starting estimate of the dates of the segment boundaries

    method.options=c('oxcal','estimate','random')
    method=tolower(method)
    match.arg(method,method.options)

    if (method=='oxcal') {
        ## use the midpoints between the oxcal dates as initial estimates
        f0=segments$date_mean+c(diff(segments$date_mean)/2,NA)
    } else if (method=='estimate') {
        t0=rep(NA,nrow(segments))
        last_date=Inf
        calfine.t=seq(from=min(x14c$Year),to=max(x14c$Year), by=0.1)
        calfine.14c=approx(x=x14c$Year,y=x14c$PMCsmoothed,xout=calfine.t)$y
        for (sidx in 1:nrow(segments)) {
            ## find dates where calibration PMC value is close to segment value
            ##fz=uniroot(function(z){ approx(x=x14c$Year,y=x14c$PMC,xout=z)$y-segments$pmc[sidx] },interval=range(x14c$Year))
            ##thisf=@(z)interp1(1:size(x,1),x(:,2)-segments(sidx,2),z);
            tempidx=which(diff(sign(calfine.14c-segments$pmc[sidx]))!=0)
            ##%tempidx(tempidx<last_date)=[];
            ##tempt=arrayfun(@(z)fzero(thisf,z),tempidx);
            tempt=calfine.t[tempidx]
            ##allt=[allt;[as_row(tempt) repmat(NaN,2-length(tempt))]];
            if (!all(tempt>last_date)) {
                tempt=tempt[tempt<last_date]
                t0[sidx]=tempt[length(tempt)]
                ##t(sidx)=interp1(1:size(x,1),x(:,1),tempt);
                last_date=t0[sidx]
            } else {
                break
            }
        }
        f0=t0+c(diff(t0)/2,NA)
    } else if (method=='random') {
        f0=sort(c(runif(nrow(segments)-1,tmin,attr(segments,"acquisition_date")),tmin+0.5),decreasing=TRUE)
    } else {
        stop(sprintf('unrecognized start option "%s"',method))
    }
    f0=c(attr(segments,"acquisition_date"),f0)
    attr(f0,"tmin")=tmin
    f0
}

