require(ggplot2)
require(dplyr)
require(readr)

read_csv('log/hdtv.csv')->hdtv
read_csv('log/its.csv')->its


library(tikzDevice)



tikz("kde.tex", width = 3.47, height = 2.8)

p<-ggplot( data = hdtv,aes(x=psi, y=rho)) +
  geom_density_2d(aes(color = ..level..),show.legend = TRUE, size=1.)+
  geom_point(data=its, aes(x=psi, y=rho,shape="Its"),size=4,show.legend = TRUE, color="black") +
  labs(color = "kde HDTV", shape="",    x = "$\\psi$",
    y = "$\\rho$")+ scale_color_viridis_c(option="A")+
  scale_shape_manual(values = 4) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Save the plot as a high-resolution image (e.g., PDF or PNG) for publication
#ggsave("kde.pdf", pl, width = 8, height = 6, dpi = 300)
print(p)
#theme_bw()
dev.off()
