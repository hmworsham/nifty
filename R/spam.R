#' Spam
#' 
#' @description Creates quick maps of U.S.
#' @importFrom mapdata map_data
#' @import ggplot2
#' @param state Character string indicating a set of colors.
#' @return Printed map of US in outline
#' @export spam.usa
#' @examples

spam.usa <- function() {
  usa <- map_data('state')
  ggplot(data=usa, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
  }

#' @description Creates quick maps of states
#' @importFrom mapdata map_data
#' @import ggplot2
#' @param state Character string indicating a set of colors.
#' @return Printed map of selected state(s) in outline
#' @export spam.state
#' @examples

spam.state <- function(states) {
  sm <- spam.usa()
  sm <- sm[sm$region %in% states]
  ggplot(data=sm, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
}

#' @description Creates quick maps of states
#' @importFrom mapdata map_data
#' @import ggplot2
#' @return Printed map of CO in outline
#' @export spam.co
#' @examples

spam.co <- function() {
  mco <- spam.state(c('colorado'))
  ggplot(data=mco, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
}

#' @description Creates quick maps of states
#' @importFrom mapdata map_data
#' @import ggplot2
#' @return Printed map of CO in outline
#' @export spam.west
#' @examples

spam.west <- function() {
  mwst <- spam.state(c(
    'california',
    'oregon', 
    'washington',
    'nevada',
    'idaho',
    'arizona',
    'new mexico', 
    'utah',
    'colorado',
    'montana',
    'wyoming'
    ))
  ggplot(data=mwest, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
}