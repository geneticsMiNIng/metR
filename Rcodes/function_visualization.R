draw_methylation <- function(DT,chrom, s, e ){
  
  DT = DT[chr == chrom & start >= s & end <= e]
  DT$met1 = DT$numCs1/DT$coverage1
  DT$met2 = DT$numCs2/DT$coverage2
  
  p <- ggplot(DT)  + xlim(s,e) 
  p <- p +  geom_point(DT,map=aes(x=start, y=met1, colour=coverage1), size = 15, shape = 20) 
  p <- p + scale_color_gradient(low="pink1", high="red4", name = "log(coverage_EK)", trans = "log")
  p <- p +  geom_point(DT,map=aes(x=start, y=met2, fill=coverage2), size = 9, shape = 21)
  p <- p + scale_fill_gradient(low="cadetblue1", high="royalblue4", name = "log(coverage EU)", trans = "log")
  
  x = as.matrix(DT[,2, with = F])
  y1 = as.matrix(DT[,'met1', with = F])
  y2 = as.matrix(DT[,'met2', with = F])
  
  p <- p + geom_segment(aes(x = x, y = y1, 
                            xend = x, yend = y2) )
  
  p <- p +  ggtitle("Methylation rate within two samples") + 
    xlab("DNA position") + ylab("Methylation rate") + 
    theme(
      plot.title = element_text(size=20, face="bold.italic"),
      axis.title.x = element_text(size=14, face="bold.italic"),
      axis.title.y = element_text(size=14, face="bold.italic"),
      legend.position="bottom", 
      axis.text.x = element_text(size=14), 
      axis.text.y = element_text(size=14), 
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12), 
      plot.margin = unit(c(1,1,1,1), "cm"))
  p
}
