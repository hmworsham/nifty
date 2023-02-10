#' spam.usa
#' @description Creates quick maps of U.S.
#' @import ggplot2
#' @param state Character string indicating a set of colors.
#' @return Printed map of US in outline
#' @export spam.usa
#' @examples
#' \dontrun{
#' spam.usa()}

spam.usa <- function() {
  usa <- map_data('state')
  ggplot(data=usa, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
  }

#' spam.state
#' @description Creates quick maps of states
#' @import ggplot2
#' @param state Character string indicating a set of colors.
#' @return Printed map of selected state(s) in outline
#' @export spam.state
#' @examples
#' \dontrun{
#' spam.state(c('california'))
#' spam.state(c('rhode island', 'new hampshire'))
#' }
#' 

spam.state <- function(st) {
  sm <- map_data('state')
  sm <- sm[sm$region %in% st,]
  ggplot(data=sm, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
}

#' spam.co
#' @description Creates quick maps of states
#' @import ggplot2
#' @return Printed map of CO in outline
#' @export spam.co
#' @examples
#' \dontrun{
#' spam.co()
#' }
#' 

spam.co <- function() {
  mco <- map_data('state')
  mco <- mco[mco$region %in% c('colorado'),]
  ggplot(data=mco, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
}

#' spam.west
#' @description Creates quick maps of states
#' @import ggplot2
#' @return Printed map of CO in outline
#' @export spam.west
#' @examples
#' \dontrun{
#' spam.west()
#' }
#' 
spam.west <- function() {
  mwst <- map_data('state')
  mwst <- mwst[mwst$region %in% c(
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
    'wyoming'),]
  ggplot(data=mwst, aes(x=long, y=lat, group=group)) + 
    geom_polygon(color='black', fill='white') + 
    coord_fixed(1.3) +
    theme_void()
}