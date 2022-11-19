library(ggsci)

colorFunct <- function(n, names = NULL, color.and.fill = FALSE, show = FALSE){

  if(n <= 8){
    fills <- c(ggsci::pal_material("red")(9)[6], ggsci::pal_material("amber")(9)[5],
               ggsci::pal_material("light-green")(9)[5], ggsci::pal_material("teal")(9)[6],
               ggsci::pal_material("light-blue")(9)[7], ggsci::pal_material("purple")(9)[4],
               ggsci::pal_material("pink")(9)[4], ggsci::pal_material("brown")(9)[7])
  }else{
    if(n <= 16){
      fills <- c(ggsci::pal_material("red")(9)[c(6, 9)], ggsci::pal_material("amber")(9)[c(5, 9)],
                 ggsci::pal_material("light-green")(9)[c(5, 9)], ggsci::pal_material("teal")(9)[c(6, 9)],
                 ggsci::pal_material("light-blue")(9)[c(7, 9)], ggsci::pal_material("purple")(9)[c(4, 9)],
                 ggsci::pal_material("pink")(9)[c(4, 9)], ggsci::pal_material("brown")(9)[c(7, 9)])
    }else{
      if(n <= 24){
        fills <- c(ggsci::pal_material("red")(9)[c(2, 6, 9)], ggsci::pal_material("amber")(9)[c(2, 5, 9)],
                   ggsci::pal_material("light-green")(9)[c(2, 5, 9)], ggsci::pal_material("teal")(9)[c(2, 6, 9)],
                   ggsci::pal_material("light-blue")(9)[c(2, 7, 9)], ggsci::pal_material("purple")(9)[c(2, 4, 9)],
                   ggsci::pal_material("pink")(9)[c(2, 4, 9)], ggsci::pal_material("brown")(9)[c(2, 7, 9)])
      }else{
        stop("Number of colors exceeds 24")
      }
    }
  }

  colors <- darken(fills, 0.3)

  fills <- fills[1:n]
  colors <- colors[1:n]

  if(!is.null(names)){
    if(n == length(names)){
      fills <- setNames(fills, names)
      colors <- setNames(colors, names)
    }else{stop("Number of colors and number of names do not match")}
  }

  if(show){
    if(color.and.fill){
      par(mfrow = c(1,2))
      show_col(fills, borders = NA); show_col(colors, borders = NA)
    }else{
      par(mfrow = c(1,1))
      show_col(fills, borders = NA)
    }
  }

  if(color.and.fill){
    return <- list(fills = fills, colors = colors)
  }else{
    return <- fills
  }

  return(return)

}
