library(ggpubr)
p <- ggbarplot(chl_perfinal, x = "Response variable", y = "Varaince explained percent",
               fill = "Explanatory variables", add = "stack",
               legend.title = "", xlab = "", ylab = "Variance explained (%)")

p <- p + scale_y_continuous(labels = scales::percent)
print(p)

my_palette <- c("#654321", "#ADD8E6", "#CD853F", "forestgreen", "steelblue", "#FFDB58",
                "#800020", "gray")


p <- ggbarplot(chl_perfinal, x = "Response variable", y = "Varaince explained percent",
               fill = "Explanatory variables", add = "stack",
               legend.title = "", xlab = "", ylab = "Variance explained (%)")

p <- p + scale_y_continuous(labels = function(x) {x * 100}) +
  scale_fill_manual(values = my_palette)

print(p)



chl_perfinal$`Explanatory variables`<- forcats::fct_relevel(chl_perfinal$`Explanatory variables`, "Residual", after = Inf)


















scatter_8 <- ggplot(chloroc, aes(hour, FvP_over_FmP)) +
  geom_point(aes(color = Day)) +
  scale_colour_viridis_d()+
  scale_size_continuous(range = c(1, 9))+
  labs(x = "Time of Day", y = "FvP over FmP", color = "Day") +
  theme_bw()+  theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.line.y = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.ticks = element_line(color = "black", size = 0.5)
  ) +
  guides(color = "none")




print(scatter_8)


scatter_6 <- ggplot(chloroc, aes(hour, NPQt)) +
  geom_point(aes(color = Day)) +
  scale_colour_viridis_d()+
  scale_size_continuous(range = c(1, 9))+
  labs(x = "Time of Day", y = "NPQt", color = "Day") +
  theme_bw()+  theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.line.y = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.ticks = element_line(color = "black", size = 0.5)
  )




print(scatter_6)


scatter_7 <- ggplot(chloroc, aes(hour, Relative_Chlorophyll)) +
  geom_point(aes(color = Day)) +
  scale_colour_viridis_d()+
  scale_size_continuous(range = c(1, 9))+
  labs(x = "Time of Day", y = "Relative Chlorophyll", color = "Day") +
  theme_bw()+  theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.line.y = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.ticks = element_line(color = "black", size = 0.5)
  ) +
  guides(color = "none")

LSjag90!
  
  
  
  print(scatter_7)

combined_scatter<- ggarrange(scatter_6, scatter_7, scatter_8)

install.packages("gridExtra")
library(gridExtra)
grid.arrange(scatter_6, scatter_7, scatter_8)
print(combined_scatter)


library(PerformanceAnalytics)
correlationc<- chart.Correlation(correlation)