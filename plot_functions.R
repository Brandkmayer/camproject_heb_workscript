## Functions for Plotting Camera data

#______________________________ 1st function is a two year comparrison using slopeplot _____________________________________

# Merge the two years in to on dataframe (df for simplification)
y2ySlope <- function(VarofInt,site,Year_1,Year_2,dfY_1,dfY_2) { 
df<- merge(dfY_1, dfY_2, by ="species", all=TRUE)
df[is.na(df)] <- 0
colnames(df) <- c("Species", "Year_1", "Year_2")
left_label <- paste(df$Species, round(df$`Year_1`,2),sep=", ")
right_label <- paste(df$Species, round(df$`Year_2`,2),sep=", ")
df$class <- ifelse((df$`Year_2` - df$`Year_1`) < 0, "red", "green")

# Plot
p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`Year_1`, yend=`Year_2`, col=class), size=.75, show.legend=F) +
  ggtitle(site) +
  theme(plot.title = element_text(hjust = 0.5,size = 15)) +
  geom_vline(xintercept=1, colour="grey", linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, colour="grey", linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="", y=VarofInt) +  # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`Year_1`, df$`Year_2`))))  # X and Y axis limits

# Add texts
p <- p + geom_text(label=left_label, y=df$`Year_1`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
p <- p + geom_text(label=right_label, y=df$`Year_2`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
p <- p + geom_text(label=Year_1, x=1, y=1.1*(max(df$`Year_1`, df$`Year_2`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label=Year_2, x=2, y=1.1*(max(df$`Year_1`, df$`Year_2`)), hjust=-0.1, size=5)  # title

# Minify theme
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))
}
#_________________________ 2nd Function is a 2 year comparision using a slope plot but by percent _________________________
y2ypct_slope <- function(VarofInt, site,Year_1, Year_2, dfY_1, dfY_2){
  for(col in names(dfY_2)[-1]) {
    dfY_2[paste0(col, "_pct")] = dfY_2[col] / sum(dfY_2[col])
  }
  for(col in names(dfY_1)[-1]) {
    dfY_1[paste0(col, "_pct")] = dfY_1[col] / sum(dfY_1[col])
  }
  # Remove count column
  dfY_2[2] <- NULL
  dfY_1[2] <- NULL
  
  df<- merge(dfY_1, dfY_2, by ="species", all=TRUE)
  df[is.na(df)] <- 0
  colnames(df) <- c("Species", "Year_1", "Year_2")
  left_label <- paste(df$Species, round(df$`Year_1`,2),sep=", ")
  right_label <- paste(df$Species, round(df$`Year_2`,2),sep=", ")
  df$class <- ifelse((df$`Year_2` - df$`Year_1`) < 0, "red", "green")
  
  # Plot
  p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`Year_1`, yend=`Year_2`, col=class), size=.75, show.legend=F) +
    ggtitle(site) +
    theme(plot.title = element_text(hjust = 0.5,size = 15)) +
    geom_vline(xintercept=1, colour="grey", linetype="dashed", size=.1) + 
    geom_vline(xintercept=2, colour="grey", linetype="dashed", size=.1) +
    scale_color_manual(labels = c("Up", "Down"), 
                       values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
    labs(x="", y=VarofInt) +  # Axis labels
    xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`Year_1`, df$`Year_2`))))  # X and Y axis limits
  
  # Add texts
  p <- p + geom_text(label=left_label, y=df$`Year_1`, x=rep(1, NROW(df)), hjust=1.1, size=3.5)
  p <- p + geom_text(label=right_label, y=df$`Year_2`, x=rep(2, NROW(df)), hjust=-0.1, size=3.5)
  p <- p + geom_text(label=Year_1, x=1, y=1.1*(max(df$`Year_1`, df$`Year_2`)), hjust=1.2, size=5)  # title
  p <- p + geom_text(label=Year_2, x=2, y=1.1*(max(df$`Year_1`, df$`Year_2`)), hjust=-0.1, size=5)  # title
  
  # Minify theme
  p + theme(panel.background = element_blank(), 
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            panel.border = element_blank(),
            plot.margin = unit(c(1,2,1,2), "cm"))
  
}

#___________________________________________________ 3rd Simple Barchart ___________________________________________________
# includes Excel base colors used in first publication by Brett
barchart4sp <- function(df, title, description, yaxis) {
  values = c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")
  df.summ <- totall %>% group_by(species) %>% summarize(Mean = mean(Time), Min = min(Time), Max = max(Time), Total = sum(Time))
  g <- ggplot(df.summ, aes(species, Total, fill = values))
  
  g + geom_bar(stat="identity", width = 0.5,fill=values) +
    labs(title=title, 
         subtitle=description, 
         caption="") +
    labs(x="Grazing Species", 
         y=yaxis) +  # Axis labels
    theme(axis.text.x = element_text(angle=65, 
                                     vjust=0.6)
    )
}
#_____________________________________________________4th Bar Chart by percent___________________________________________________
barchartpctsp <- function(df, title, description ) {
  for(col in names(df)[-1]) {
    df[paste0(col, "_pct")] = df[col] / sum(df[col])
  }
  # Remove count column
  df[2] <- NULL
  # Cow = "#ED7D31", Elk = "#A5A5A5", Horse = "#4472C4", Muledeer = "#FFC000"
  values = c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")
  # Create a plot
  g <- ggplot(df, aes(species, freq_pct, fill = values))
  # Fill the plot
  g + geom_bar(stat="identity", width = 0.5,fill=values) +
    labs(title=title, 
         subtitle=description, 
         caption="") +
    labs(x="Grazing Species", 
         y="Detections") +  # Axis labels
    theme(axis.text.x = element_text(angle=65, 
                                     vjust=0.6)
    )
}

#_________________________________________________________5th Boxplot _______________________________________________________
# Boxplot for plotting duration of visit. 
# Sauce: https://stackoverflow.com/questions/15071334/boxplot-of-table-using-ggplot2
# Sauce: https://stackoverflow.com/questions/14563531/combine-column-to-remove-nas
box_grztime <- function(df){
  values = c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")
  ggplot(data = melt(df), aes(x=species, y= value)) + geom_boxplot(aes(fill=species)) +
    stat_summary(fun.y=mean, colour="darkred", geom="point", 
                 shape=18, size=3,show_guide = FALSE) + 
    geom_text(data = means, aes(label = weight, y = weight + 0.08))
  
}
#___________________________________________________ 6th Boxplot with mean __________________________________________________
box_grztime_wmean <- function(df){
  df.means <- df %>% group_by(species) %>% summarize(Mean = ceiling(mean(Time)))
  values = c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")
  ggplot(data = melt(df), aes(x=species, y= value)) + geom_boxplot(aes(fill=species)) +
    stat_summary(fun.y=mean, colour="darkred", geom="point", 
                 shape=18, size=3,show_guide = FALSE) + 
    geom_text(data = df.means, aes(label = Mean, y = Mean - 50.08), nudge_x = 0.08)
  
}

#__________________________________________________7th Barchart with error bars _________________________________________________
df <- BRL17totgrpgrztme
barchart4sp_werr <- function(df, title, description, yaxis ) {
  values = c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")
  df.summ <- df %>% group_by(species) %>% summarize(Mean = ceiling(mean(Time)), Min = min(Time), Max = max(Time), Rise = Max/Mean[is.finite(Max/Mean)])
  
  g <- ggplot(df.summ, aes(x = species, y = Mean, ymin = Min, ymax = Max, fill = values))
  
  g + geom_bar(stat="identity", width = 0.5,fill=values) +
    geom_errorbar(width = 0.3)+
    labs(title=title, 
         subtitle=description, 
         caption="") +
    labs(x="Grazing Species", 
         y=yaxis) +  # Axis labels
    geom_text(data = df.summ, aes(label = Mean, y = Mean + (Max/Mean), nudge_x = 0.115)) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)
    )
}

# _____________________________________________ 8th Grouped Barchart __________________________________________________________

# BY PERCENT
# to be used in conjunction with Behavioral_frequency 4 column data.frame.
# includes Horse, cattle, elk, deer and is plotted to show 2 sites. Can be manipulated to add more
barchart_bhvpctfrq <- function(df, site_name) {
  g <-ggplot(df, aes(fill=Species, y=Value, x=Behavior)) + geom_bar(data = df, position="dodge", stat="identity")
  g <- g + theme(panel.background = element_rect(fill = "white"),
                 panel.grid.major.y = element_line(colour = "grey70")
  ) + scale_fill_manual(values=c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")) + ggtitle(site_name) + theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(name="Frequency", breaks=seq(0,1,0.05))
}
# BY TOTAL COUNT
barchart_bhvtotfrq <- function(df, site_name) {
  g <-ggplot(df, aes(fill=Species, y=Value, x=Behavior)) + geom_bar(data = df, position="dodge", stat="identity")
  g + theme(panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(colour = "grey70")
  ) + scale_fill_manual(values=c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")) + ggtitle(site_name) + theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(name="Frequency", breaks=seq(0,1,0.05))
}
### Paired group barcharts for year to year comparison of means by site
paired_barchart <- function(df, site_name) {
  df.summ <- df %>% 
    group_by(year, species) %>% 
    summarize(Mean = ceiling(mean(Time)), Min = min(Time), Max = max(Time), Total = sum(Time))
  ggplot(df.summ, aes(fill=year, y=Mean, x=species, ymin = Min ,ymax = Max)) + 
    geom_bar(position="dodge", stat="identity") + 
    geom_errorbar(width = 0.4, position = position_dodge(.9)) +
    labs(x="Grazing Species", 
         y= "Time (min)") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey70")
    ) + scale_fill_manual(values=c("#0072B2", "#D55E00")) + 
    ggtitle(site_name) + theme(plot.title = element_text(hjust = 0.5))
} 
### Paired group barcharts for year to year comparison of means betweeen sites
sitepaired_barchart <- function(df) {
  df.summ <- df %>% 
    group_by(site, year, species) %>% 
    summarize(Mean = ceiling(mean(Time)), Min = min(Time), Max = max(Time), Total = sum(Time))
  ggplot(df.summ, aes(fill=year, y=Mean, x=species, ymin = Min ,ymax = Max)) + 
    facet_wrap(~site) + 
    geom_bar(position="dodge", stat="identity") + 
    labs(x="Grazing Species", 
         y= "Time (min)") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey70")
    ) + scale_fill_manual(values=c("#0072B2", "#D55E00")) + 
    ggtitle("Comparison of Heber Site Group Visit Duration by Year") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_y_continuous(name="Time (min)", breaks=seq(0,100,5))
}
### Paired group barcharts for year to year comparison of means betweeen sites
# With error bars
sitepaired_barchart_we <- function(df) {
  df.summ <- multsitegrpgrz %>% 
    group_by(site, year, species) %>% 
    summarize(Mean = ceiling(mean(Time)), Min = min(Time), Max = max(Time), Total = sum(Time))
  ggplot(df.summ, aes(fill=year, y=Mean, x=species, ymin = Min ,ymax = Max)) + 
    facet_wrap(~site) + 
    geom_bar(position="dodge", stat="identity") + 
    geom_errorbar(width = 0.4, position = position_dodge(.9)) +
    labs(x="Grazing Species", 
         y= "Time (min)") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey70")
    ) + scale_fill_manual(values=c("#0072B2", "#D55E00")) + 
    ggtitle("Comparison of Heber Site Group Visit Duration by Year") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_y_continuous(name="Frequency", breaks=seq(0,100,10))
}
### Comparing sites 
sitepaired_bhv_barchart <- function(df) {
  df.summ <- df  %>% 
    group_by(Site, Species, Behavior) %>% 
    summarize(Total = sum(Value))
  ggplot(df.summ, aes(fill=Species, y=Total, x=Behavior)) + 
    facet_wrap(~Site) + 
    geom_bar(position="dodge", stat="identity") + 
    labs(x="Grazing Species", 
         y= "Time (min)") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey70")
    ) + scale_fill_manual(values=c("#4472C4", "#ED7D31","#A5A5A5", "#FFC000")) + 
    ggtitle("Counts of Group Behavior by Site") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_y_continuous(name="Counts", breaks=seq(0,150,10))
}
#_______________________________________________________________________________________________________________
# binding multiple graphs useing ggarrange
bind2graphs <- function(g1, g2) {
  ggarrange(g1, g2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
}
bind3graphs <- function(g1, g2, g3) {
  ggarrange(g1, g2, g3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
}
bind4graphs <- function(g1, g2, g3, g4) {
  ggarrange(g1, g2, g3, g4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
}

