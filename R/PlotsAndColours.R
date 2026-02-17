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
#' @param discrete Boolean indicating whether the scale is discrete or not
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

#' Colour Scale constructor for HEU/NHS themes
#'
#' @param palette the name of the NHS or HEU palette (defaults to 'heu')
#' @param discrete Boolean indicating whether the palette is discrete or not
#' @param reverse Boolean indicating whether the colour order should be reversed
#' @param ... Additional parameters to be passed to discrete_scale or scale_colour_gradientn() respectively for discrete TRUE/FALSE
#'
#' @importFrom ggplot2 discrete_scale scale_colour_gradientn
#' @export
scale_colour_heu <- function(palette='heu',discrete=TRUE,reverse=FALSE,...){
    pal <- get_heu_palette(palette=palette,reverse=reverse)
    if(discrete){
        discrete_scale("colour",paste0("nhstheme_",palette),palette=pal,...)
    } else {
        scale_fill_gradientn(colours=pal(256),...)
    }
}


#' HEU Standard theme
#'
#' @param font the name of the font family for the plot text
#' @param titlesize the pointsize for the main title
#' @param subtitlesize the point size for the subtitle
#' @param captionsize the point size for the caption
#' @param axistitlesize point size for the axis title
#' @param axistextsize point size for the axis text
#'
#'
#' @importFrom ggplot2 theme theme_minimal
#' @export
theme_heu <- function(font = "sans",titlesize=20,subtitlesize=14,captionsize=9,axistitlesize=10,axistextsize=9){

    theme_minimal() %+replace%    #replace elements we want to change
        theme(

              #grid elements
              panel.grid.major = element_blank(),    #strip major gridlines
              panel.grid.minor = element_blank(),    #strip minor gridlines
              axis.ticks = element_blank(),          #strip axis ticks

              #since theme_minimal() already strips axis lines, 
              #we don't need to do that again

              #text elements
              plot.title = element_text(             #title
                                        family = font,            #set font family
                                        size = titlesize,         #set font size
                                        face = 'bold',            #bold typeface
                                        hjust = 0,                #left align
                                        vjust = 2),               #raise slightly

              plot.subtitle = element_text(          #subtitle
                                           family = font,            #font family
                                           size = subtitlesize),     #font size

              plot.caption = element_text(           #caption
                                          family = font,            #font family
                                          size = captionsize,                 #font size
                                          hjust = 1),               #right align

              axis.title = element_text(             #axis titles
                                        family = font,            #font family
                                        size = axistitlesize),               #font size

              axis.text = element_text(              #axis text
                                       family = font,            #axis famuly
                                       size = axistextsize),                #font size

              axis.text.x = element_text(            #margin for axis text
                                         margin=margin(5, b = 10))
        )
}


#' Helper function to convert RGB colour values to a hex triplet.
#'
#' @param r red colour value (0-255)
#' @param g green colour value
#' @param b blue colour value
#' @export
rgb2hex <- function (r, g, b) 
{
    sprintf("#%s", paste(format(as.hexmode(c(r, g, b)), width = 2), 
        collapse = ""))
}
