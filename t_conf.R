# Load the libraries
library(ggplot2)
library(dplyr)
library(ggtext)
library(latex2exp)

t_conf(0.91757, 9, 0.05,0.25,'greater')

# Function for both tail normal distribution ------------------------------
t_conf <- function(t, df, los, y_ann, side, xlab = 'x', ylab = 'Probability', title = 'Confidence Interval'){
  # t: Value of the test statistic
  # df: degree of freedom 
  # ltail: left point of the random variable
  # rtail: Right point of the random variable
  # los: Level of significance
  # side: two.sided, les, greater
  # Shaded region function ------------------------
  shaded_region <- function(df, start, end, fill, alpha){
    geom_area(data = subset(df, x > start & x < end),
              aes(y = prob, fill = fill), color = NA, alpha = alpha)
  }
  # Terminal points -------------------------------
  ltail <- -4
  rtail <- 4
  # Making synthetic data -------------------------
  data <- data.frame(x = seq(ltail,rtail,0.01),
                     prob = dt(seq(ltail,rtail,0.01), df = df))
  # Creating plot object --------------------------
  plot <- data %>%
    ggplot(aes(x = x, y = prob)) + geom_line()
  # Tail condition ----------------------------------
  if (side == "two.sided"){
    # Assign some boundary points -------------------
    left_cric <- round(qt(los/2, df = df), 2)
    right_cric <- round(qt(1-(los/2),df = df), 2)
    t <- round(t, 2)
    p_value <- round(2*pt(abs(t), df = df, lower.tail = FALSE), 5)
    # Annotation data -------------------------------
    data_ann <- data.frame(x = c(left_cric, right_cric, t), y = y_ann,
                           labels = c(paste('Left Critical point = ', left_cric),
                                      paste('Right Critical point = ', right_cric),
                                      paste('Test statistic = ', t)))
    # Plotting --------------------------------------
    plot <- plot +
      # vertical dashed line
      geom_vline(aes(xintercept = left_cric), col = 'red', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      geom_vline(aes(xintercept = right_cric), col = 'red', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      geom_vline(aes(xintercept = t), col = 'blue', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      geom_vline(aes(xintercept = -abs(t)), col = 'blue', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      # Separate shaded region
      shaded_region(data, ltail, left_cric, 'Rejection Region', 0.2) +
      shaded_region(data, right_cric, rtail, 'Rejection Region', 0.2) +
      shaded_region(data, left_cric, right_cric, 'Acceptance Region', 0.2) +
      shaded_region(data, ltail, -abs(t), 'P-value', 0.5) +
      shaded_region(data, abs(t), rtail, 'P-value', 0.5)
    
  } else if (side == "less") {
    # Assign some boundary points -------------------
    left_cric <- round(qt(los, df = df), 2)
    t <- round(t, 2)
    p_value <- round(pt(t, df = df, lower.tail = TRUE), 5)
    # Annotation data -------------------------------
    data_ann <- data.frame(x = c(left_cric, t), y = y_ann,
                           labels = c(paste('Critical point = ', left_cric),
                                      paste('Test statistic = ', t)))
    # Plotting --------------------------------------
    plot <- plot +
      # vertical dashed line
      geom_vline(aes(xintercept = left_cric), col = 'red', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      geom_vline(aes(xintercept = t), col = 'blue', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      # Separate shaded region
      shaded_region(data, ltail, left_cric, 'Rejection Region', 0.2) +
      shaded_region(data, left_cric, rtail, 'Acceptance Region', 0.2) +
      shaded_region(data, ltail, t, 'P-value', 0.5)
    
  } else if (side == "greater"){
    right_cric <- round(qt(1-(los), df = df), 2)
    p_value <- round(pt(t, df = df, lower.tail = FALSE), 5)
    # Annotation data -------------------------------
    data_ann <- data.frame(x = c(right_cric, t), y = y_ann,
                           labels = c(paste('Critical point = ', right_cric),
                                      paste('Test statistic = ', round(t, 2))))
    # Plotting --------------------------------------
    plot <- plot +
      # vertical dashed line
      geom_vline(aes(xintercept = right_cric), col = 'red', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      geom_vline(aes(xintercept = t), col = 'blue', size = 0.8, alpha = 0.2, linetype = 'dashed') +
      # Separate shaded region
      shaded_region(data, right_cric, rtail, 'Rejection Region', 0.2) +
      shaded_region(data, ltail, right_cric, 'Acceptance Region', 0.2) +
      shaded_region(data, t, rtail, 'P-value', 0.5)
    
  } else {
    print('Wrong input for side argument!')
  }
  # decision rule ------------------------------------
  decision <- ifelse(p_value < los, 'Reject $H_0$', 'Failed to reject $H_0$')
  plot <- plot +
    # manual color filling
    scale_fill_manual(name = 'Region',
                      breaks = c('Rejection Region', 'Acceptance Region', 'P-value'),
                      values = c('red', 'green', gray(0.40))) +
    # axis title (Labs)
    labs(x = xlab, y = ylab, title = title) +
    # printing lables with the box
    ggtext::geom_richtext(data = data_ann, aes(x = x, y = y, label = labels), angle = 90, size = 3) +
    # annotations
    annotate('text', 3, 0.12, label = paste('P-value', p_value)) +
    annotate('text', 3, 0.09, label = TeX(decision), col = ifelse(p_value < los, 'red', 'green3')) +
    # setting up theme and guide legend
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'top') +
    guides(fill = guide_legend(override.aes = list(alpha = 0.15)))+
    expand_limits(y = c(0,0.5))
  return(plot)
}

p <- t_conf(3.9835, 21, 0.05,0.4,'two.sided', title = '(Variances are assumed to be equal)')
ggsave('./My Pictures/plot3b.png', p, device = 'png')


