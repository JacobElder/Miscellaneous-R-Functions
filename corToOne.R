corToOne <- function(inputDf, referenceVar){
  requiredPackages <- c("tidyverse", "corrr", "ggplot2")
  attachedPackages <- requiredPackages %in% (.packages())
  whichUnattached <- which(attachedPackages==FALSE)
  if(length(whichUnattached)==0){
    # Nothing
    print("All required packages attached")
  }else {
    lapply(requiredPackages[whichUnattached], require, character.only = TRUE)
  }
  
  output <- inputDf %>% 
    correlate() %>% 
    focus(referenceVar) %>%
    arrange(across(starts_with(referenceVar)))
  colnames(output)[1] <- c("Variables")
  return(output)
}

plotCorToOne <- function(inputDf, referenceVar){
  corDf <- corToOne(inputDf, referenceVar)
  
  corDf %>% 
    mutate(Variables = factor(Variables, levels = Variables[order( get(referenceVar)  )])) %>%  # Order by correlation strength
    ggplot(aes(x = Variables, y = get(referenceVar) )) +
    geom_bar(stat = "identity") +
    ylab("Correlation Coefficient") +
    xlab("Individual Differences") + theme_grey(base_size = 9)  + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
    theme(axis.text.x = element_text( 
      size = 9, angle = 45, vjust = 1)) + theme(axis.title.x = element_text(vjust=1.9)) + theme(axis.text=element_text(size=9),
                                                                                                axis.title=element_text(size=9,face="bold")) + theme(legend.text = element_text(size=9)) + theme(panel.border = element_rect(colour = "black", fill = NA, size =1)) + theme(legend.title = element_blank()) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
}