################################################################################
#####################Correlation vs. dependence plot to ggplot2 ################
################################################################################
#Info --------------------------------------------------------------------------
#showing (in)dependent with different correlation values
#used for slides-2-filter slide 4 (Filter: Pearson & Spearman correlation)
#source: https://en.wikipedia.org/wiki/Correlation_and_dependence#/media/File:Correlation_examples2.svg
# Carolin Becker, 28.07.2020

#!!!!
#As it is random, correlation might not show the "perfect" value (e.g. 0 last row)
#--> run again


#Libraries----------------------------------------------------------------------
library(ggplot2)
library(mvtnorm)
library(dplyr)
library(tidyr)
library(scales)

#Functions----------------------------------------------------------------------

#' @description rotates data with "rotation" (radian)
#' @param data the and y values which should be rotated
#' @param rotation in radians
#' @return data matrix data with rotation 
rotate <- function (data, rotation){
  data %*% matrix(c(cos(rotation), sin(rotation), -sin(rotation), cos(rotation)), ncol = 2)
}


# For the first two rows: 

#' @description creates data frame with column form, x and y for the plot 
#' (different n, rotation, correlation, rotation and name of the form)
#' @param n number of rows
#' @param cor correlation between the data points
#' @param rotation rotation of the data in radians
#' @param name name of the form (should be different for facet_wrap)
#' @return data.frame with n rows and columns form, x and y (specified n, rotation, correlation)
linear_data_frame <- function (n = 400, cor = 1, rotation = 0, name = paste0("rot", rotation, "cor", cor)){
  #make covariance matrix
  sd = matrix(c(1,cor,cor,1), ncol = 2)
  data <- rmvnorm(n, c(0, 0), sd)
  if(rotation != 0){data <- rotate(data = data, rotation = rotation)}
  data.frame(form = name,
             x = data[,1], 
             y = data[,2])
}


#Variables----------------------------------------------------------------------

#number of points per graph 
n <- 1000

x <- runif(n, -1, 1)
y <- runif(n, -1, 1)

#square 
square <- rotate(data = cbind(x,y), rotation =  -pi/8 )

#rotate square
square_rotated <- rotate(data = square, rotation =  -pi/8)

variance_4 <- 0.1
four_circles <- rbind(
  xy1 = rmvnorm(n/4, mean = c( 1,  1), sigma = matrix(c(variance_4,0,0,variance_4), ncol= 2)),
  xy2 = rmvnorm(n/4, mean = c( -1,  1), sigma = matrix(c(variance_4,0,0,variance_4), ncol= 2)),
  xy3 = rmvnorm(n/4, mean = c( -1,  -1), sigma = matrix(c(variance_4,0,0,variance_4), ncol= 2)),
  xy4 = rmvnorm(n/4, mean = c( 1,  -1), sigma = matrix(c(variance_4,0,0,variance_4), ncol= 2)))


#create dataframe with the different datasets
data_forms <- rbind(
  ################################
  #changing correlation
  ################################
  
  linear_data_frame (n = n, cor = 1, rotation = 0),
  linear_data_frame (n = n, cor = 0.8, rotation = 0),
  linear_data_frame (n = n, cor = 0.4, rotation = 0),
  linear_data_frame (n = n, cor = 0, rotation = 0),
  linear_data_frame (n = n, cor = -0.4, rotation = 0),
  linear_data_frame (n = n, cor = -0.8, rotation = 0),
  linear_data_frame (n = n, cor = -1, rotation = 0),
  
  
  
  ################################
  #changing slope
  ################################
  linear_data_frame (n = n, cor = 1, rotation = 0, name= "cor1rot0_seccondrow"),
  linear_data_frame (n = n, cor = 1, rotation = pi/12),
  linear_data_frame (n = n, cor = 1, rotation = pi/6),
  linear_data_frame (n = n, cor = 1, rotation = pi/4),
  linear_data_frame (n = n, cor = 1, rotation = pi/2-pi/6),
  linear_data_frame (n = n, cor = 1, rotation = pi/2-pi/12),
  linear_data_frame (n = n, cor = 1, rotation = pi/2),
  
  ################################
  #forms from mutual information 
  ################################
  data.frame(form = "w", 
            x = x, 
            y = 4 * (x^2 - 1/2)^2 + runif(n, -1, 1)/3),
  data.frame(form = "square",
            x = square[,1], 
            y = square[,2]),
  data.frame(form = "square_rotated",
             x = square_rotated[,1], 
             y = square_rotated[,2]),
  #runif(n, -(1-abs(xnorm)), 1-abs(xnorm))), #xnorm <- rnorm(n, 0, 0.5)
  data.frame(form = "u",
             x = x, 
             y = 2*x^2 + runif(n, -1, 1)),
  data.frame(form = "cross",
             x = x, 
             y = (x^2 + runif(n, 0, 1/2)) * sample(seq(-1, 1, 2), n, replace = TRUE)),
  data.frame(form = "circle",
             x = sin(x*pi) + rnorm(n, 0, 1/8), 
             y = cos(x*pi) + rnorm(n, 0, 1/8)),
   data.frame(form = "four_circles",
              x = four_circles[,1],
              y = four_circles[,2]))

#factorize forms to keep the order of the forms
data_forms$form <- factor(x = data_forms$form, levels= unique(data_forms$form))

#calculate correlation per group of data (per from)
correlation <- data_forms %>% 
  group_by(form) %>% 
  summarize(label = paste0("r=",as.character(round(replace_na(cor(x, y),0),digits = 1)))) 


#Plot---------------------------------------------------------------------------
ggplot(data_forms, aes(x = x, y = y)) +
  geom_point(alpha = .2) +
  #show all graphs
  facet_wrap( ~ form, ncol = 7,labeller = function(...) "", scales = "free_x") +
  #remove legend and title pane of facet_wrap
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  #add labels
  geom_label(data = correlation, aes(x = 0, y = 1.8, label = label), size= 7) + 
  ylim(c(-2,2)) +
  scale_x_continuous(breaks= pretty_breaks())+theme(axis.text=element_text(size=12),
                                                    axis.title=element_text(size=14,face="bold"))
  # scale_x_continuous(
  #   breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
  #   expand = expand_scale(mult = c(0, 0.05))
  # )
#Save---------------------------------------------------------------------------

ggsave(filename = file.path("figure_man","chunk2_filter_correlation.png"))



