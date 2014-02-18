#' Read sample data file
#'
#' @param filename string: the Excel file to read
#' @param sheetname string: the name of the worksheet to read. If NULL, all worksheets are checked and the first one with a header row is used
#' @param sample_column_names string: the column names that are used to identify data rows within the datasheet
#' @param columns_to_return string: the columns to extract from the datasheet
#'
#' @return A data frame of results
#' @export

read_segment_data=function(filename,sheetname=NULL,sample_column_names=c("Lab ID", "Moss species", "Sample ID"),columns_to_return=c("Lab ID","Moss species","Sample ID","Depth (mm)","Segment length (mm)","pMC","pMC_sd")) {
    ## these column names define a sample. We look for these names to find our header row. Also, these are used to decide when the data-table part of the sheet finishes (the first row where all three of these columns are blank, after the header row)

    if (nchar(sheetname)==0 || is.null(sheetname)) {
        ## if no sheet specified, find all sheets in the workbook
        workbook=loadWorkbook(filename)
        sheetname=getSheets(workbook)
    }

    for (sheet_index in 1:length(sheetname)) {
        this_sheet=readWorksheetFromFile(filename, sheet = sheetname[sheet_index], header = FALSE)
        this_sheet_full=this_sheet ## keep copy of the full thing, too
        ## iterate through rows looking for sample_column_names
        is_header_row=apply(this_sheet,1,function(z)all(tolower(sample_column_names) %in% tolower(str_trim(z)))) ## will be TRUE where all sample_column_names appear in this row
        if (any(is_header_row)) {
            ## we have found a header row, so exit from the loop
            break
        }
        ## otherwise continue looping to the next sheet
    }
    if (!any(is_header_row)) {
        stop("header row not found in data file")
    }
    if (sum(is_header_row)>1) {
        stop("multiple rows in data sheet appear to be header rows")
    }
    ## if we got this far, we have one header row
    sheet_header=this_sheet[is_header_row,]
    sheet_header=sapply(sheet_header,str_trim) ## trim off whitespaces
    ## find the end of the data, which we assume is the first row (after the header row) with all-blank entries for sample_column_names
    this_sheet=this_sheet[(which(is_header_row)+1):nrow(this_sheet),] ## first row below header row, and below
    columns_to_check=which(tolower(sheet_header) %in% tolower(sample_column_names))
    first_blank_row=min(which(apply(this_sheet[,columns_to_check],1,function(z)all(is.na(z))))) ## first all-blank row
    if (is.infinite(first_blank_row)) {
        warning("No blank rows found in data sheet, am assuming that the data rows extend to the bottom of the sheet")
    } else {
        this_sheet=this_sheet[1:(first_blank_row-1),]
    }
    columns_idx=tolower(sheet_header) %in% tolower(columns_to_return)

    this_sheet=this_sheet[,columns_idx]
    names(this_sheet)=tolower(sheet_header[columns_idx])

    ## also extract date and location collected, and add as attributes of this_sheet
    idx=which(this_sheet_full=="Date collected",arr.ind=TRUE)
    ## if not found, throw an error
    if (nrow(idx)<1) {
        stop('"Date collected" not found in data sheet')
    }
    if (nrow(idx)>1) {
        stop('Multiple "Date collected" cells found in data sheet')
    }
    acquisition_date=this_sheet_full[idx[1,1]+1,idx[1,2]]
    ## convert to decimal year
    acquisition_date=strptime(acquisition_date,format=getOption("XLConnect.dateTimeFormat"))
    acquisition_date=as.numeric(strftime(acquisition_date,'%Y'))+as.numeric(strftime(acquisition_date,'%j'))/365.25
    attr(this_sheet,"acquisition_date")=acquisition_date ## set as an attribute of the segments object

    ## also extract acqusition location. We don't actually use this (yet), so set to NA if not found but don't throw an error
    idx=which(this_sheet_full=="Longitude",arr.ind=TRUE)
    if (nrow(idx)==1) {
        acquisition_lon=this_sheet_full[idx[1,1]+1,idx[1,2]]
        attr(this_sheet,"acquisition_lon")=acquisition_lon
    } else {
        attr(this_sheet,"acquisition_lon")=NA
    }
    idx=which(this_sheet_full=="Latitude",arr.ind=TRUE)
    if (nrow(idx)==1) {
        acquisition_lat=this_sheet_full[idx[1,1]+1,idx[1,2]]
        attr(this_sheet,"acquisition_lat")=acquisition_lat
    } else {
        attr(this_sheet,"acquisition_lat")=NA
    }

    this_sheet
}
