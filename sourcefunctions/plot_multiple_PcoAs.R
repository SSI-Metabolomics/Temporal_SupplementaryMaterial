plot_PcoA <- function (batches, md, distmetric = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), collow = "#810f7c", colhigh ="#f7fcb9", cat = "plate", catcols = "plate", mdtype = 'categorical', cols = c('grey','black','orange','pink','darkgreen','darkred','brown','blue','skyblue3','green'), titles = c('title 1', 'title 2', 'title 3'), path_plots = "your_path_here", name_plot = "name_here") {
  
  plots <- list()
  count <- 0
  local(for (i in 1:length(batches)){
    
    count <<- count + 1
    
    # not true if we do not match the rows in the batche tables as I have done in the previous steps
    # if (is.null(rownames(batches[[i]])) == TRUE){
    #   rownames(batches[[i]]) <- rownames(datExpr_scaled)
    # }
    
    # if (blinclude == FALSE){
    #   batches[[i]] <- batches[[i]][-which(rownames(batches[[i]]) %in% blanksamples),]
    # }
    
    # if (remnonstudysamples == TRUE){
    #   batches[[i]] <- batches[[i]][-which(rownames(batches[[i]]) %in% nonstudysamples),]
    # }
    
# distm <- coda.base::dist(batches[[i]], method = distmetric)
    distm <- dist(batches[[i]], method = distmetric)
    
    if (length(which(is.na(as.matrix(distm))==TRUE)) != 0){
      
      d <- as.matrix(distm)
      d[is.na(d)] <- 1 # if distance between two samples is NA, replace with 0, e.g. blanks with all zero features
      distm <- as.dist(d)
      
    }
    
    md <- md[which(rownames(md)%in%rownames(batches[[i]])),]
    md <- md[match(rownames(batches[[i]]),rownames(md)),]
    
    
    if (length(which(is.na(md[,colnames(md) == cat]))) !=0){
      red <- as.dist(as.matrix(distm)[-which(is.na(md[,colnames(md) == cat])),-which(is.na(md[,colnames(md) == cat]))])
      #Rsq <- adonis(red ~ na.omit(md[,colnames(md) == cat]))$aov.tab$R2[1]
      #pval <- adonis(red ~ na.omit(md[,colnames(md) == cat]))$aov.tab$'Pr(>F)'[1]
      adonres <- adonis(red ~ na.omit(md[,colnames(md) == cat]))
      Rsq <- adonres$aov.tab$R2[1]
      pval <- adonres$aov.tab$'Pr(>F)'[1]
    } else {
      #Rsq <- adonis(distm ~ md[,colnames(md) == cat])$aov.tab$R2[1]
      #pval <- adonis(distm ~ md[,colnames(md) == cat])$aov.tab$'Pr(>F)'[1]
      adonres <- adonis(distm ~ md[,colnames(md) == cat])
      Rsq <- adonres$aov.tab$R2[1]
      pval <- adonres$aov.tab$'Pr(>F)'[1]
    }
    
    PcoA <- cmdscale(distm, k =2)
    PcoA <- as.data.frame(PcoA)
    PcoA$Group <- md[,colnames(md) == catcols][match(rownames(PcoA),rownames(md))]
    names(PcoA)[1:2] <- c('PC1', 'PC2')
    
    # plot
    cbPalette <- cols
    
    if (mdtype == 'continuous'){
      Tr_PcoA <- ggplot(PcoA, aes(x = PC1, y = PC2, colour = Group, label = row.names(PcoA))) + geom_point(size=2.5) +
        #scale_color_npg()+
        scale_colour_gradient(low = collow, high = colhigh,space = "Lab", na.value = "grey50", guide = "colourbar",aesthetics = "colour")+
        theme(legend.title=element_blank()) +
        ggtitle(paste(titles[i],paste0("(p=",round(pval,4),","),paste0("adonis-R2=",round(Rsq,4),")"),sep=" "))
      
    } else {
      
      Tr_PcoA <- ggplot(PcoA, aes(x = PC1, y = PC2, colour = Group, label = row.names(PcoA))) + geom_point(size=2.5) +
        #scale_color_npg()+
        scale_colour_manual(values = cbPalette) +
        theme(legend.title=element_blank()) +
        ggtitle(paste(titles[i],paste0("(p=",round(pval,4),","),paste0("adonis-R2=",round(Rsq,4),")"),sep=" "))
    }
    Tr_PcoA
    #print(Tr_PcoA)
    plots[[count]] <<- Tr_PcoA
  })
  pdf(paste(path_plots, name_plot, sep = '/'),width=20,height=20) 
  grid.arrange(grobs = plots, ncol = 3, widths =c(4,4,4))
  dev.off()
  }