#' Summarize fitted date estimates
#'
#' @param s dataframe: fitted date estimates, as returned by fit_dates()
#' @param cal dataframe: the calibration data, as returned by read_calibration()
#' @param segments dataframe: the segment data, as returned by read_segment_data()
#'
#' @return A dataframe. If segments is non-null, the summary values are added to that data frame
#' @export

fit_summary=function(s,cal,segments=NULL) {
    ## Summary table
    fhat=as.data.frame(t(apply(s,2,function(x) c(mn=mean(x),sd=sd(x),quantile(x,c(0.5,0.025,0.975))))))
    seg_c14=seg_c14fun(cal)
    ## pull out estimated c14 per segment, mean segment date, and growth rate
    fhat.c14=seg_c14(fhat$mn)
    fhat.mid_date=fhat$mn[1:(length(fhat$mn)-1)]+diff(fhat$mn)/2 ## mean segment date (i.e. date of segment midpoint)
    if (is.null(segments)) {
        fhat
    } else {
        ## add fhat info to segments data frame, and return that
        segments$fitted.date_segment_end=fhat$mn[1:(length(fhat$mn)-1)]
        segments$fitted.date_segment_mid=fhat.mid_date
        segments$fitted.date_segment_start=fhat$mn[2:(length(fhat$mn))]
        segments$fitted.c14=fhat.c14
        segments
    }
}

