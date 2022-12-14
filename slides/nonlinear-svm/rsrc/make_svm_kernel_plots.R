# ------------------------------------------------------------------------------
# FIG: SVM KERNEL PLOTS
# ------------------------------------------------------------------------------

library(ggplot2)
library(viridis)
library(mlr)
library(gridExtra)

source("utils.R")

############################################################################

x <- cbind(
  c(2,3,4,5,6,0,1,2,3,1,2,3),
  c(0,0,0,0,0,0,1,2,3,-1,-2,-3))

#class
y <- c(-1,-1,-1,-1,-1,1,1,1,1,1,1,1)

#create data frame
df <- data.frame (x = x, y = as.factor(y))

#make task
tsk <- makeClassifTask(data = df, target = 'y')

p_svm_linear_kernel <- plotSVM(tsk, list(kernel = "linear",
                                nu = 0.2, 
                                tolerance = 0.001, 
                                cost = 1, 
                                type = "C-classification", 
                                scale = FALSE)) + 
  ggtitle("")+
  xlab(expression(x[1]))+
  ylab(expression (x[2]))

ggsave("../figure/svm_linear_kernel.png", plot = p_svm_linear_kernel, width = 6, height = 2.5)


############################################################################

p_svm_poly_kernel_d2 <- plotSVM(tsk, list(kernel = "polynomial", degree = 2, coef0 = 1, gamma = 1)) +
  ggtitle("d = 2") +
  xlab(expression(x[1])) +
  ylab(expression (x[2]))
p_svm_poly_kernel_d3 <- plotSVM(tsk, list(kernel = "polynomial", degree = 3, coef0 = 1, gamma = 1)) +
  ggtitle("d = 3") +
  xlab(expression(x[1])) +
  ylab(expression (x[2]))

p_svm_poly_kernel <- grid.arrange(p_svm_poly_kernel_d2, p_svm_poly_kernel_d3, ncol= 2)

ggsave("../figure/svm_poly_kernel.png", plot = p_svm_poly_kernel, width = 6, height = 2.5)

############################################################################

gamma <- 1
x.coord <- seq(-4, 4, length=250)
data_points <- data.frame(x.1= c(1.5,1), x.2= c(0.75,1))
distance <- sqrt((data_points[1,1]-data_points[2,1])^2+(data_points[1,2]-data_points[2,2])^2)
distance

data <- data.frame(
  distance = x.coord,
  k = exp( - gamma * x.coord^2)
)

distance_plot <- ggplot(data = data_points, aes(x.1,x.2)) +
  geom_point() +
  geom_line(color='red') + 
  annotate("text", x = 1, y = 0.6, label = expression(italic(r) == "||"* x - tilde(x)*"||"^{2}), parse = TRUE, color = 'red', size = 5) +
  geom_text(label= c("x", expression(tilde(x))),hjust=1.5, vjust=1.5, size = 5)+
  scale_x_continuous(expand = c(0, 0), limits = c(0,2)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.25)) +
  xlab(expression(bold(x[1]))) +
  ylab(expression (bold(x[2])))+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=14, face="bold", colour = "black"), 
    axis.text.y = element_text(size=14, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 10, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3)
  ) +
  theme_bw()


rbf_plot <- ggplot(data = data, aes(x=distance, y=k))+ 
  geom_line()+ 
  geom_vline(xintercept=distance, color = 'red')+
  xlab ("r") +
  ylab(expression(bold(exp( - gamma  ~~ "||" ~~ r ~~ "||"^2))))+
  theme(
    # LABLES APPEARANCE
    plot.title = element_text(size=14, face= "bold", colour= "black" ),
    axis.title.x = element_text(size=14, face="bold", colour = "black"),    
    axis.title.y = element_text(size=14, face="bold", colour = "black"),    
    axis.text.x = element_text(size=14, face="bold", colour = "black"), 
    axis.text.y = element_text(size=14, face="bold", colour = "black"), # bold
    strip.text.x = element_text(size = 10, face="bold", colour = "black" ),
    strip.text.y = element_text(size = 10, face="bold", colour = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3)
  ) +
  theme_bw()

rbf <- plotSVM(tsk, list(kernel = "radial", gamma = 1)) +
  ggtitle("") +
  xlab(expression(x[1])) +
  ylab(expression (x[2]))

p_svm_rbf_kernel <- grid.arrange(distance_plot, rbf_plot,rbf,  ncol = 3, widths = c(2,2,3))

ggsave("../figure/svm_rbf_kernel.png", plot = p_svm_rbf_kernel, width = 9, height = 3)
