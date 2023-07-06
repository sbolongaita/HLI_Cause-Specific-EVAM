groupColor <- function(n, names = NULL, palettes = NULL, color.and.fill = FALSE, show = FALSE){

  if(is.null(palettes)){
    palettes <- c("red", "amber", "light-green", "teal", "light-blue",
                  "purple", "pink", "brown", "grey")
  }

  n_palettes <- length(n)

  fills <- character()
  for(i in 1:n_palettes){
    if(n[i] <= 9){
      if(n[i] == 1){sample <- 6}
      if(n[i] == 2){sample <- c(4, 8)}
      if(n[i] == 3){sample <- c(4, 6, 8)}
      if(n[i] == 4){sample <- c(3, 5, 7, 9)}
      if(n[i] == 5){sample <- c(3, 4, 5, 7, 9)}
      if(n[i] == 6){sample <- c(3, 4, 5, 6, 7, 9)}
      if(n[i] == 7){sample <- c(3, 4, 5, 6, 7, 8, 9)}
      if(n[i] == 8){sample <- c(2, 3, 4, 5, 6, 7, 8, 9)}
      if(n[i] == 9){sample <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)}
      fills <- append(fills, ggsci::pal_material(palettes[i])(9)[sample])
    }else{
      n_lighter <- ceiling(n[i] / 2)
      lighter <- grDevices::colorRampPalette(ggsci::pal_material("light-green")(9)[1:8])(n_lighter)
      n_darker <- n[i] - n_lighter
      darken <- n_darker / (n_lighter * 1.5)
      darker <- grDevices::colorRampPalette(c(ggsci::pal_material("light-green")(9)[9],
                                              colorspace::darken(ggsci::pal_material("light-green")(9)[9], darken)))(n_darker)
      sample <- c(lighter, darker)
      fills <- append(fills, sample)
    }
  }

  colors <- colorspace::darken(fills, 0.3)

  if(!is.null(names)){
    if(sum(n) == length(names)){
      fills <- stats::setNames(fills, names)
      colors <- stats::setNames(colors, names)
    }else{stop("Number of colors and number of names do not match")}
  }

  if(show){
    if(color.and.fill){
      graphics::par(mfrow = c(1,2))
      scales::show_col(fills, borders = NA); scales::show_col(colors, borders = NA)
    }else{
      graphics::par(mfrow = c(1,1))
      scales::show_col(fills, borders = NA)
    }
  }

  if(color.and.fill){
    return <- list(fills = fills, colors = colors)
  }else{
    return <- fills
  }

  return(return)

}
