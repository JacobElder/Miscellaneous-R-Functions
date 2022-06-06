plotCommAxes <- function(plot1, plot2, xaxis, yaxis){
  
  arranged <- ggarrange(plot1 + rremove("ylab") + rremove("xlab"), plot2 + rremove("ylab") + rremove("xlab"), ncol =2, nrow = 1, hjust=.42, common.legend = T)
  addedAxes <- annotate_figure(arranged, left = textGrob(yaxis, rot = 90, vjust = 1, hjust = .4, gp = gpar(cex = 1.3, fontsize=10)),
                               bottom = textGrob(xaxis, vjust = -.25, gp = gpar(cex = 1.3, fontsize=10))
  )
  return(addedAxes)
  
}