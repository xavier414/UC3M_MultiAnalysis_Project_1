plothist <- function(col_name, df, color_1) {
  
  density_col <- density(df[[col_name]],
                         kernel="gaussian")
  
  # Min-Max x axis
  min_x_d <- min(density_col$x)
  max_x_d <- max(density_col$x)
  
  # Min-Max y axis 
  min_y_d <- min(density_col$y)
  max_y_d <- max(density_col$y)
  
  par(mfrow=c(1,2))
  hist(df[[col_name]],
       main = paste("Histogram of", col_name),
       xlab = col_name,
       col = color_1,
       freq = FALSE,
       xlim = c(min_x_d, max_x_d),
       ylim = c(min_y_d, max_y_d)
  )
  lines(density_col,
        ylab="Density",
        col="blue",
        lwd=2)
  boxplot(df[[col_name]],
          main= paste("Boxplot of", col_name),
          xlab= col_name,
          ylab="All Colleges",
          col=color_1,
          horizontal=TRUE)
  par(mfrow=c(1,1))
  
}