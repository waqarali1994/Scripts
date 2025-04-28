library(PerformanceAnalytics)
chart.Correlation(chloro_traits)
install.packages("ggpubr")
library(ggpubr)
ggscatter(chloro_traits, aes(x= NPQt, y = FvP_over_FmP))+ geom_point()
subset_chloro <- chloro_traits[, c("NPQt", "FvP_over_FmP")]

scatter <- ggplot(subset_chloro1, aes(x = NPQt, y = Leaf_Temperature)) +
  geom_point()
print(scatter)
subset_chloro1 <- chloro_traits[, c("NPQt", "Leaf_Temperature")]
install.packages("GGally")
library(GGally)
ggpairs(chloro_traits)

ggplot(chloro_traits, aes(x = NPQt, y = FvP_over_FmP, color = Relative_Chlorophyll, shape = Leaf_Temperature, size = Light_Intensity)) +
  geom_point() +
  scale_shape_binned() +
  scale_color_gradient(low = "red", high = "green")

scatter5 <- ggplot(chloro_traits, aes(x = NPQt, y = Relative_Chlorophyll, color = Leaf_Temperature)) +
  geom_point() +
  scale_color_gradient(low = "red", high = "green")

scatter3 <- ggplot(chloroc, aes(x = hour, y = NPQt, color = Day)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_gradient(low = "red", high = "green", name = "Day") +
  labs(title = "",
       x = "hour",
       y = "NPQt")
theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_line(color = "grey90", linetype = "dotted")) +
  geom_vline(xintercept = 0.0000001, linetype="solid", color = "black", size = 0.5) +
  geom_hline(yintercept = 0.1, linetype="solid", color = "black", size = 0.5)

print(scatter3)


scatter_matrix<- ggpairs(
  data = chloroc,
  columns = 3:13,
  mapping = ggplot2::aes(color = "priming"),
  upper = list(continuous = "cor"), 
  lower = list(continuous = "points"), 
  diag = list(continuous = "densityDiag"))
print(scatter_matrix)

scattert_matrix2<- pairs(x = chloroc[1:13],
                         main = "A matrix of scatterplots",
                         pch = 21, 
                         bg = c("red", "green3", "blue"))


scatter_4<- ggplot(chloroc, aes(hour, NPQt, color=Day))+ geom_jitter()
print(scatter_4)

install.packages("viridis")
library(viridis)
scatter_2<- ggplot(chloroc, aes(hour, NPQt))+
  geom_point(aes(color=Day))+
  scale_size_continuous(range = c(1,9))
print(scatter_5)

scatter_7<- ggplot(chloroc, aes(hour, NPQt)) +
  geom_point(aes(color = Day)) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 9))

print(scatter_5)

scatter_6<- factor(chloroc$Day)

scatter_5 <- ggplot(chloroc, aes(hour, NPQt)) +
  geom_point(aes(color = Day)) +
  scale_colour_viridis_d()+
  scale_size_continuous(range = c(1, 9))+
  labs(x = "Hour", y = "NPQt", color = "Day") +
  theme_bw()+  theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.line.y = element_line(color = "black", size = 0.5, linetype = "solid"),
                     axis.ticks = element_line(color = "black", size = 0.5)
  )




print(scatter_5)


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
  )




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
  )




print(scatter_7)

combined_scatter<- ggarrange(scatter_6, scatter_7, scatter_8)

print(combined_scatter)
