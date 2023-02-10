#' icolors
#' Vectors of colors for figures
#' @description Creates different vectors of related colors that may be useful for figures.
#' @param set Character string indicating a set of colors.
#' @return Vector of character strings representing the chosen set of colors, in RGB.
#' @importFrom grDevices rgb2hsv
#' @importFrom stats hclust dist
#' @importFrom graphics par rect text
#' @seealso [plot_crayons()]
#' @keywords utilities
#' @examples
#' \dontrun{
#' points(1, 8, pch=21, bg=icolors('main'), cex=4)
#' }
#' @export

icolors <-
  function(set = c(
    'general',
    'general2',
    'bg',
    'bgpng',
    'cmyk',
    'CC',
    'CCalt',
    'f2',
    'sex',
    'main',
    'crayons',
    'web',
    'summer21',
    'pantone',
    'mario',
    'gothic'
  ))
  {
    general <- c(
      'lightblue' = rgb(102, 203, 254, maxColorValue = 255),
      'hotpink' = rgb(254,  0, 128, maxColorValue = 255),
      'pink' = rgb(254, 102, 254, maxColorValue = 255),
      'green' = rgb(102, 254, 102, maxColorValue = 255),
      'purple' = rgb(128,  0, 128, maxColorValue = 255),
      'lightpurple' = rgb(203, 102, 254, maxColorValue = 255),
      'yellow' = rgb(254, 203, 102, maxColorValue = 255),
      'darkblue' = rgb(0, 128, 128, maxColorValue = 255)
    )
    
    general2 <- c(
      blue = '#7B68ED',
      green = '#1B9E78',
      orange = '#E59E00',
      red = '#ca3767'
    )
    
    bg <- c('bg' = rgb(24, 24, 24, maxColorValue = 255))
    bgpng <- c('bgpng' = rgb(32, 32, 32, maxColorValue = 255))
    
    # cmyk printing
    cmyk <- c(
      'c' = rgb(0, 255, 255, maxColorValue = 255),
      'm' = rgb(255, 0, 255, maxColorValue = 255),
      'y' = rgb(255, 255, 0, maxColorValue = 255),
      'k' = rgb(0, 0, 0, maxColorValue = 255)
    )
    
    # https://compgen.unc.edu/wp/?page_id=577
    CC <- c(
      'AJ'  = rgb(240, 240,  0, maxColorValue = 255),
      'B6'  = rgb(128, 128, 128, maxColorValue = 255),
      '129' = rgb(240, 128, 128, maxColorValue = 255),
      'NOD' = rgb(16, 16, 240, maxColorValue = 255),
      'NZO' = rgb(0, 160, 240, maxColorValue = 255),
      'CAST' = rgb(0, 160,  0, maxColorValue = 255),
      'PWK' = rgb(240,  0,  0, maxColorValue = 255),
      'WSB' = rgb(144,  0, 224, maxColorValue = 255)
    )
    
    # improved CC colors, after https://clrs.cc/
    CCalt <- c(
      'AJ'  = '#FFDC00',
      'B6'  = '#888888',
      '129' = '#F08080',
      'NOD' = '#0064C9',
      'NZO' = '#7FDBFF',
      'CAST' = '#2ECC40',
      'PWK' = '#FF4136',
      'WSB' = '#B10DC9'
    )
    
    f2 <- c(
      'AA' = as.character(CCalt[1]),
      'AB' = rgb(0, 200, 0, maxColorValue = 255),
      'BB' = as.character(CCalt[4]),
      'error' = '#cdc5c2'
    )
    
    sex <- c(female = rgb(255, 80, 80, maxColorValue = 255),
             male = as.character(CCalt[4]))
    
    main <- c('main' = rgb(0, 64, 128, maxColorValue = 255))
    
    crayons = c(
      'Almond' = '#efdecd',
      'Antique Brass' = '#cd9575',
      'Apricot' = '#fdd9b5',
      'Aquamarine' = '#78dbe2',
      'Asparagus' = '#87a96b',
      'Atomic Tangerine' = '#ffa474',
      'Banana Mania' = '#fae7b5',
      'Beaver' = '#9f8170',
      'Bittersweet' = '#fd7c6e',
      'Black' = '#000000',
      'Blizzard Blue' = '#ace5ee',
      'Blue' = '#1f75fe',
      'Blue Bell' = '#a2a2d0',
      'Blue Gray' = '#6699cc',
      'Blue Green' = '#0d98ba',
      'Blue Violet' = '#7366bd',
      'Blush' = '#de5d83',
      'Brick Red' = '#cb4154',
      'Brown' = '#b4674d',
      'Burnt Orange' = '#ff7f49',
      'Burnt Sienna' = '#ea7e5d',
      'Cadet Blue' = '#b0b7c6',
      'Canary' = '#ffff99',
      'Caribbean Green' = '#00CC99',
      'Carnation Pink' = '#ffaacc',
      'Cerise' = '#dd4492',
      'Cerulean' = '#1dacd6',
      'Chestnut' = '#bc5d58',
      'Copper' = '#dd9475',
      'Cornflower' = '#9aceeb',
      'Cotton Candy' = '#ffbcd9',
      'Dandelion' = '#fddb6d',
      'Denim' = '#2b6cc4',
      'Desert Sand' = '#efcdb8',
      'Eggplant' = '#6e5160',
      'Electric Lime' = '#ceff1d',
      'Fern' = '#71bc78',
      'Forest Green' = '#6dae81',
      'Fuchsia' = '#c364c5',
      'Fuzzy Wuzzy' = '#cc6666',
      'Gold' = '#e7c697',
      'Goldenrod' = '#fcd975',
      'Granny Smith Apple' = '#a8e4a0',
      'Gray' = '#95918c',
      'Green' = '#1cac78',
      'Green Blue' = '#1164b4',
      'Green Yellow' = '#f0e891',
      'Hot Magenta' = '#ff1dce',
      'Inchworm' = '#b2ec5d',
      'Indigo' = '#5d76cb',
      'Jazzberry Jam' = '#ca3767',
      'Jungle Green' = '#3bb08f',
      'Laser Lemon' = '#fefe22',
      'Lavender' = '#fcb4d5',
      'Lemon Yellow' = '#fff44f',
      'Macaroni and Cheese' = '#ffbd88',
      'Magenta' = '#f664af',
      'Magic Mint' = '#aaf0d1',
      'Mahogany' = '#cd4a4c',
      'Maize' = '#edd19c',
      'Manatee' = '#979aaa',
      'Mango Tango' = '#ff8243',
      'Maroon' = '#c8385a',
      'Mauvelous' = '#ef98aa',
      'Melon' = '#fdbcb4',
      'Midnight Blue' = '#1a4876',
      'Mountain Meadow' = '#30ba8f',
      'Mulberry' = '#c54b8c',
      'Navy Blue' = '#1974d2',
      'Neon Carrot' = '#ffa343',
      'Olive Green' = '#bab86c',
      'Orange' = '#ff7538',
      'Orange Red' = '#ff2b2b',
      'Orange Yellow' = '#f8d568',
      'Orchid' = '#e6a8d7',
      'Outer Space' = '#414a4c',
      'Outrageous Orange' = '#ff6e4a',
      'Pacific Blue' = '#1ca9c9',
      'Peach' = '#ffcfab',
      'Periwinkle' = '#c5d0e6',
      'Piggy Pink' = '#fddde6',
      'Pine Green' = '#158078',
      'Pink Flamingo' = '#fc74fd',
      'Pink Sherbert' = '#f78fa7',
      'Plum' = '#8e4585',
      'Purple Heart' = '#7442c8',
      'Purple Mountains Majesty' = '#9d81ba',
      'Purple Pizzazz' = '#fe4eda',
      'Radical Red' = '#ff496c',
      'Raw Sienna' = '#d68a59',
      'Raw Umber' = '#714b23',
      'Razzle Dazzle Rose' = '#ff48d0',
      'Razzmatazz' = '#e3256b',
      'Red' = '#ee204d',
      'Red Orange' = '#ff5349',
      'Red Violet' = '#c0448f',
      'Robins Egg Blue' = '#1fcecb',
      'Royal Purple' = '#7851a9',
      'Salmon' = '#ff9baa',
      'Scarlet' = '#fc2847',
      'Screamin Green' = '#76ff7a',
      'Sea Green' = '#93dfb8',
      'Sepia' = '#a5694f',
      'Shadow' = '#8a795d',
      'Shamrock' = '#45cea2',
      'Shocking Pink' = '#fb7efd',
      'Silver' = '#cdc5c2',
      'Sky Blue' = '#80daeb',
      'Spring Green' = '#eceabe',
      'Sunglow' = '#ffcf48',
      'Sunset Orange' = '#fd5e53',
      'Tan' = '#faa76c',
      'Teal Blue' = '#18a7b5',
      'Thistle' = '#ebc7df',
      'Tickle Me Pink' = '#fc89ac',
      'Timberwolf' = '#dbd7d2',
      'Tropical Rain Forest' = '#17806d',
      'Tumbleweed' = '#deaa88',
      'Turquoise Blue' = '#77dde7',
      'Unmellow Yellow' = '#ffff66',
      'Violet (Purple)' = '#926eae',
      'Violet Blue' = '#324ab2',
      'Violet Red' = '#f75394',
      'Vivid Tangerine' = '#ffa089',
      'Vivid Violet' = '#8f509d',
      'White' = '#FFFFFF',
      'Wild Blue Yonder' = '#a2add0',
      'Wild Strawberry' = '#ff43a4',
      'Wild Watermelon' = '#fc6c85',
      'Wisteria' = '#cda4de',
      'Yellow' = '#fce883',
      'Yellow Green' = '#c5e384',
      'Yellow Orange' = '#ffae42'
    )
    
    web <- c(
      'navy' = '#001f3f',
      # from https://clrs.cc
      'blue' = '#0074d9',
      'aqua' = '#7fdbff',
      'teal' = '#39cccc',
      'olive' = '#3d9970',
      'green' = '#2ecc40',
      'lime' = '#01ff70',
      'yellow' = '#ffdc00',
      'orange' = '#ff851b',
      'red' = '#ff4136',
      'maroon' = '#85144b',
      'fuschia' = '#f012be',
      'purple' = '#b10dc9',
      'black' = '#111111',
      'gray' = '#aaaaaa',
      'silver' = '#dddddd'
    )
    
    summer21 = c(
      'marigold' = '#FBAD5A',
      'cerulean' = '#99B3D0',
      'rust' = '#B25E47',
      'illuminating' = '#F2DD5A',
      'frenchblue' = '#357AB2',
      'greenash' = '#A2DAAE',
      'burntcoral' = '#EA8B86',
      'mint' = '#18A67A',
      'orchid' = '#926BA7',
      'raspberry' = '#CD4A77'
    )
    
    pantone = c(
      'Cerulean' = '#9BB7D4',
      'Fuchsia' = '#C74375',
      'Truth' = '#BF1932',
      'Aqua' = '#7BC4C4',
      'Tigerlily' = '#E2583E',
      'Blue' = '#53B0AE',
      'Sand' = '#DECDBE',
      'Chili' = '#9B1B30',
      'Blue' = '#5A5B9F',
      'Mimosa' = '#F0C05A',
      'Turquoise' = '#45B5AA',
      'Honeysuckle' = '#D94F70',
      'Tangerine' = '#DD4124',
      'Emerald' = '#009473',
      'Radiant' = '#B163A3',
      'Marsala' = '#955251',
      'Rose' = '#F7CAC9',
      'Serenity' = '#92A8D1',
      'Greenery' = '#88B04B',
      'Ultra' = '#5F4B8B',
      'Living' = '#FF6F61',
      'Classic' = '#0F4C81',
      'Ultimate' = '#939597',
      'Illuminating' = '#F5DF4D',
      'Very' = '#6667AB'
    )
    
    mario = c(
      'mario' = '#E20B21',
      'luigi' = '#17A442',
      'peach' = '#EC518E',
      'yoshi' = '#89BD29',
      'blue' = '#0C5CA4',
      'ltblue' = '#36BBE2',
      'yellow' = '#F9F335',
      'tan' = '#F6B767',
      'brown' = '#8C492A',
      'brick' = '#BD3F1C',
      'cloud' = '#F2F2F2',
      'moustache' = '#030102'
    )
    
    gothic = c(
      'whiterock' = '#C9CAD4',
      'scree' = '#D2C6BE',
      'sandstone' = '#C5A5A6',
      'teocali' = '#8984A3',
      'granite' = '#5F7382',
      'lightsky' = '#83A9DC',
      'darksky' = '#2E4874',
      'spruce' = '#1D332B',
      'sage' = '#637955',
      'aspen' = '#B0CA73',
      'veratrum' = '#F0E178',
      'soil' = '#A0834D'
    )
    
    switch(
      match.arg(set),
      general = general,
      general2 = general2,
      bg = bg,
      bgpng = bgpng,
      cmyk = cmyk,
      CC = CC,
      CCalt = CCalt,
      f2 = f2,
      sex = sex,
      main = main,
      crayons = crayons,
      web = web,
      summer21 = summer21,
      pantone = pantone,
      mario = mario,
      gothic = gothic
    )
    
  }

#' icolors.plot
#' Illustration of icolors
#' Creates plots of the colors in [icolors()]
#' @param method2order method to order colors (`'hsv'` or `'cluster'`)
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see [graphics::par()])
#' @param bg Background color
#' @param fg Foreground color (for text and box outlines)
#' @param border If TRUE, plot a border around each rectangle
#' @return None
#' @export
#' @references <https://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors>
#' @seealso [icolors()]
#' @examples
#' \dontrun{
#' icolors.plot()
#' }

icolors.plot <-
  function(set = c(
    'general',
    'general2',
    'bg',
    'bgpng',
    'cmyk',
    'CC',
    'CCalt',
    'f2',
    'sex',
    'main',
    'crayons',
    'web',
    'summer21',
    'pantone',
    'mario',
    ''
  ),
  method2order = c('orig', 'hsv', 'cluster'),
  cex = 0.6,
  mar = rep(1, 4),
  bg = 'white',
  fg = 'black',
  border = T) {
    method2order <- match.arg(method2order)
    
    setname <- set
    set <- icolors(set)
    
    # get rgb
    colval <- col2rgb(set)
    
    if (method2order == 'hsv') {
      # convert to hsv
      colval <- t(rgb2hsv(colval))
      
      # order the colors; first two lines are to get black/gray/silver/white first
      ord <- order(
        names(set) != 'Black',
        names(set) != 'Gray',
        names(set) != 'Silver',
        names(set) != 'White',
        colval[, 1],
        colval[, 2],
        colval[, 3]
      )
      
    } else if (method2order == 'cluster') {
      ord <- hclust(dist(t(colval)))$ord
      
    } else {
      ord <- seq(1, length(colval))
    }
    
    oldmar <- par('mar')
    oldfg <- par('fg')
    oldbg <- par('bg')
    on.exit(par(mar = oldmar, fg = oldfg, bg = oldbg))
    
    par(mar = mar, fg = fg, bg = bg)
    divr = seq(length(set))
    divr = divr[length(set) %% divr == 0]
    if (all(divr == c(1, length(set)))) {
      divr = ceiling(length(set) / 2)
    } else {
      divr = max(divr[divr > 1 & divr < length(set)])
    }
    wid = ceiling(length(set) / divr)
    ht = ceiling(length(set) / wid)
    x <- (1:wid) - 1
    y <- (1:ht) - 1
    x <- rep(x, each = ht)
    y <- rep(y, wid)
    
    plot(
      0,
      0,
      main = setname[1],
      type = 'n',
      xlab = '',
      ylab = '',
      xaxs = 'i',
      yaxs = 'i',
      xlim = c(0, max(x) + 1),
      ylim = c(max(y) + 0.5,-0.5),
      xaxt = 'n',
      yaxt = 'n'
    )
    
    dx <- 0.2
    dy <- 0.4
    
    if (border)
      border <- fg
    else
      border <- set[ord]
    
    rect(x + dx / 4, y - dy, x + dx, y + dy,
         border = border, col = set[ord])
    text(x + dx * 1.2,
         y,
         names(set)[ord],
         cex = cex,
         adj = c(0, 0.5))
  }

#' icolors.lookup
#' Vector of colors in colorsets
#'
#' @param color_names Optional vector of color names; can be partial matches.
#' @param ... Additional optional color names
#' @return Vector of named RGB colors
#' @references <https://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors>
#' @seealso [icolors.plot()], [icolors()]
#' @export
#' @keywords utilities
#' @examples 
#' \dontrun{
#' icolors.lookup('red')
#' }
#' 
icolors.lookup <-
  function(color_names = NULL, ...)
  {
    sets <-
      c(
        'general',
        'general2',
        'bg',
        'bgpng',
        'cmyk',
        'CC',
        'CCalt',
        'f2',
        'sex',
        'main',
        'crayons',
        'web',
        'summer21',
        'pantone',
        'mario',
        'gothic'
      )
    
    if (is.null(color_names))
      return(sets)
    
    dots <- list(...)
    color_names <- unlist(c(color_names, dots))
    
    allcolors <- unlist(lapply(sets, icolors))
    allnames <- names(allcolors)
    allvals <- as.vector(allcolors)
    
    # look for exact matches
    #m <- match(color_names, allnames)
    m <- allcolors[color_names]
    
    # if items not found: do grep
    #    require exactly one match
    notfound <- color_names[is.na(m)]
    
    g <- vapply(notfound, function(a) {
      z <- grep(a, allnames, ignore.case = TRUE)
      if (length(z) < 1)
        return(-1) # not found
      if (length(z) > 1)
        return(-2) # found multiply
      if (length(z) == 1)
        return(as.integer(z))
      z
    }, 1)
    
    # issue warning if some not found or some found multiply
    if (any(g < 0)) {
      if (any(g == -1))
        warning('Some colors not found')
      if (any(g == -2))
        warning('Some colors have multiple matches')
    }
    
    g[g > 0] <- allvals[g[g > 0]]
    g[g == '-1'] <- NA
    g[g == '-2'] <- 'Multiple matches found'
    m[is.na(m)] <- g
    
    result <- m
    
    # for those matched, add input as names
    
    
    # for those not found singly, add input as names
    names(result[is.na(result)]) <- names(g[is.na(g)])
    
    result
  }