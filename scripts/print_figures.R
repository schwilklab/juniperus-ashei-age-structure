## manuscript_figures.R
## Alex Bowers

### saves figures for southwest naturalist manuscipt

fig1
ggsave("results/figure1.jpeg", plot = fig1, 
       width = 190, height = 160, dpi = 1200, units = "mm")


fig2
ggsave("results/figure2.jpeg", plot = fig3, 
       width = 160, height = 160, dpi = 1200, units = "mm")

fig3
ggsave("results/figure3.jpeg", plot = fig4, 
       width = 160, height = 200, dpi = 1200, units = "mm")

fig4
ggsave("results/figure4.jpeg", plot = fig6, 
       width = 160, height = 200, dpi = 1200, units = "mm")

fig5
ggsave("results/figure5.jpeg", plot = fig6, 
       width = 160, height = 160, dpi = 1200, units = "mm")
