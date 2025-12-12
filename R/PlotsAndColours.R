#  This file handles all the code related to plotting and colours
#'
#



#' Return a list of colours which align to the HEU standard colours
#'
#' @importFrom NHSRtheme get_nhs_colours
#' @export
get_heu_colours <- function(){
    NHSColours <- NHSRtheme::get_nhs_colours()
    NHSColours[c('AquaGreen','DarkBlue','MidGrey','LightBlue')]
}


#' Return a function to interpolate the selected palette of NHS / HEU Colours, this can call back into the standard NHS palettes from NHSRtheme
#'
#' @param palette Character name of either "heu" or the section name in the Nhs Theme
#' @param reverse Whether the colours should be ordered in reverse or not
#' @param ... Additional parameters to pass to colorRampPalette or get_nhs_palette
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom NHSRtheme get_nhs_palette
#' @export
get_heu_palette <- function(palette="heu",reverse=FALSE,...){
    if(palette=="heu"){
        pal <- get_heu_colours()
        if(reverse) pal <- rev(pal)
        pal <- colorRampPalette(pal,...)
    } else {
       pal <- get_nhs_palette(palette=palette,reverse=reverse,...)
    } 

    pal
}


#' Fill scale constructor function for HEU/NHS themes
#'
#' @param palette the name of the NHS or HEU palette (defaults to 'heu')
#' @param discrete Boolean indicating whether the scale is discreete or not
#' @param reverse Boolean indicating whether to reverse the order of the colours
#' @param ...  Additional parameters to be passed to discrete_scale() or scale_fill_gradientn() respectively for discrete TRUE/FALSE
#'
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @export
scale_fill_heu <- function(palette="heu",discrete=TRUE,reverse=FALSE,...){
    if(palette=="heu"){
        pal = get_heu_palette()
    } else {
        get_nhs_palette(palette=palette,reverse=reverse)
    }

    if(discrete){
        discrete_scale("fill",paste0("nhstheme_",palette),palette=pal,...)
    } else {
        scale_fill_gradientn(colours=pal(256),...)
    }
}
