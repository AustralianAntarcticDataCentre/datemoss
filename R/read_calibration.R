#' Read atmospheric 14C calibration data file
#'
#' @param filename string: the file to read. If NULL, the atmospheric data from Hua et al. (2013) is used. File is expected to be csv-format, with the columns "Year" and "PMC"
#' @param presmooth.width numeric: (experimental) apply sliding mean to the PMC values, with this window width
#' @param fudge.factor numeric: (experimental) scale the PMC values by this value
#'
#' @return A data frame of results
#' @examples \dontrun{cal=read_calibration()}
#' @export

read_calibration=function(filename=NULL,presmooth.width=NA,fudge.factor=NA) {
    if (is.null(filename)) {
        filename=system.file("extdata", "atmospheric_14C.csv", package="datemoss")
        x14c=read.table(filename,sep=',',header=TRUE)
        ## add extrapolated data for 2012 and 2013 so we don't hit missing data problems
        ## dates are being dealt with as fractional year values
        x14c=rbind(x14c,data.frame(Year=c(2012,2013),PMC=c(105.08,104.79),D14C=NA,D14C.sigma=NA,F14C=NA,F14C.sigma=NA))
    } else {
        x14c=read.table(filename,sep=',',header=TRUE)
    }
    if (!is.na(fudge.factor)) {
        x14c$PMC=x14c$PMC*fudge.factor
    }
    if (!is.na(presmooth.width) & presmooth.width>0) {
        ## interpolate to get evenly one-year spaced data for smoothing, with extrapolation forwards in time to avoid missing points during smoothing
        temp.year=1455:2100
        temp.PMC=approxExtrap(x14c$Year,x14c$PMC,temp.year)$y
        temp.PMCsmoothed=rollmean(temp.PMC,presmooth.width,fill=NA)
        x14c$PMCsmoothed=approx(temp.year,temp.PMCsmoothed,x14c$Year)$y
    } else {
        x14c$PMCsmoothed=x14c$PMC
    }
    okidx=!is.na(x14c$PMCsmoothed)
    x14c=x14c[okidx,]

    x14c$cumPMC=cumsum(x14c$PMCsmoothed) ## cumulative sum of PMC, for integral purposes
    atm_cumc14=approxfun(x=x14c$Year,y=x14c$cumPMC) ## convenience function to return atmospheric PMC values given date

    list(atm_cumc14=atm_cumc14, x14c=x14c)
}

