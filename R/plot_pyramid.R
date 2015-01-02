#' @title Plot population pyramid
#' @export
#' 
plot_pyramid = function(pyramid, colors = c("blue","red","darkblue","darkred"), xlab = "Population size", ylab = "Ages", xlim = NULL)
{
  females = pyramid@females
  males = pyramid@males
  ages  = pyramid@ages
  if(is.null(xlim)) 
  {
    xlim = max(c(females, males))
    xlim = c(-xlim, xlim)
  }
  # surplus
  surplus = females - males
  m_sp = surplus; m_sp[surplus > 0] = 0
  f_sp = surplus; f_sp[surplus < 0] = 0
  
  
  males   = -males - m_sp
  females = females - f_sp
  
  
  
  ylim = c(0,length(ages))
  
  nages = length(ages)
  y0 = 0:(nages-1)
  y1 = 1:nages
  
  plot(1,1, type = "n", xlim = xlim, ylim = ylim, yaxt = "n", xlab = xlab, ylab = ylab)
  abline(h = c(y0, tail(y1,1)), lty = 2, col = "gray50")
  axis(2, at = y1-0.5, ages, las = 1)
  
  #females
  rect(ybottom = y0, ytop = y1, xleft = 0, xright = females, col = colors[1])
  rect(ybottom = y0, ytop = y1, xleft = females, xright = females + f_sp, col = colors[3])
  
  #males
  rect(ybottom = y0, ytop = y1, xleft = 0, xright = males, col = colors[2])
  rect(ybottom = y0, ytop = y1, xleft = males, xright = males - m_sp, col = colors[4])
}


