#' Read sample data file
#'
#' @param filename string: the Excel file to read
#' @param sheetname string: the name of the worksheet to read. If NULL, all worksheets are checked and the first one with a header row is used
#' @param sample_column_names string: the column names that are used to identify data rows within the datasheet
#' @param columns_to_return string: the columns to extract from the datasheet. If NULL, any column that does not have an NA header value is returned
#'
#' @return A data frame of results
#' @export

read_segment_data=function(filename,sheetname=NULL,sample_column_names=c("Lab ID", "Moss species", "Sample ID"),columns_to_return=NULL) {

    if (nchar(sheetname)==0 || is.null(sheetname)) {
        ## if no sheet specified, find all sheets in the workbook
        workbook=loadWorkbook(filename)
        sheetname=getSheets(workbook)
    }

    ## the columns sample_column_names define a sample. We look for these names to find our header row. Also, these are used to decide when the data-table part of the sheet finishes (the first row where all three of these columns are blank, after the header row)
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
    if (!is.null(columns_to_return)) {
        columns_idx=tolower(sheet_header) %in% tolower(columns_to_return)
    } else {
        ##return all columns that do no have an NA header
        columns_idx=!is.na(sheet_header)
    }

    this_sheet=this_sheet[,columns_idx]
    sheet_header=tolower(sheet_header[columns_idx]) ## lower case
    sheet_header=str_replace_all(sheet_header,"[[:blank:][:punct:]]+","_") ## replace brackets, spaces, and other special characters with underscore
    sheet_header=str_replace(sheet_header,"_$","") ## remove trailing underscores
    sheet_header=str_replace(sheet_header,"^_","") ## remove leading underscores
    names(this_sheet)=sheet_header
    ## convert columns to appropriate data type: without this step, they will all be char because XLConnect has read the entire worksheet including the header row, so no columns will appear to be e.g. numeric
    for (ci in 1:ncol(this_sheet)) {
        this_sheet[,ci]=type.convert(this_sheet[,ci])
        ## coerce some types to be more generic
        if (is.integer(this_sheet[,ci])) {
            this_sheet[,ci]=as.numeric(this_sheet[,ci])
        }
        ## maybe also change factors back to strings, but leave as-is for now
    }

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
