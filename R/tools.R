#############################################################################################################
##
##	File:			tools.R
##	Author:			Simon Chambers
##	Date:			2026-02-17 17:06
##	Description:	Contains general tools functions
##				
#############################################################################################################



#' Get the NHS Sharepoint folder name
#'
#' @export

nhs_sharepoint_path <- function(){
    dirname(Sys.getenv('onedrivecommercial'))
}

#' Path to a specific project folder
#'
#' @param Folder Main folder location within the project structure. 
#' @param sub Subfolder location, default is 2.Current which is the current working folder (see sharepoint folder locations)
#'
#' @export
heu_project <- function(Folder,sub='2. Current'){    
    path <- file.path(nhs_sharepoint_path(),sub,gsub('\\\\','/',Folder))
    path
}
