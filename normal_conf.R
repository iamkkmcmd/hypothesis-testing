# Load the libraries
library(ggplot2)
library(dplyr)
library(ggtext)

# Function for both tail normal distribution ------------------------------
normal_conf <- function(t, mean, sd, los, y_ann, side, xlab = 'x', ylab = 'Probability', title = 'Confidence Interval'){
  # t: Value of the test statistic
  # mean: Mean of the distribution
  # sd: Standard deviation of the distribution
  # ltail: left point of the random variable
  # rtail: Right point of the random variable
  # los: Level of significance
  # Shaded region function ------------------------
  shaded_region <- function(df, start, end, fill, alpha){
    geom_area(data = subset(df, x > start & x < end),
              aes(y = prob, fill = fill), color = NA, alpha = alpha)
  }
  # Terminal points -------------------------------
  ltail <- mean - (3.5*sd)
  rtail <- mean + (3.5*sd)
  # Making synthetic data -------------------------
  data <- data.frame(x = seq(ltail,rtail,0.01),
                     prob = dnorm(seq(ltail,rtail,0.01), mean = mean, sd = sd))
  # Creating plot object --------------------------
  plot <- data %>%
    ggplot(aes(x = x, y = prob)) + geom_line()
  # Tail condition ----------------------------------
  if (side == "two.sided"){
    # Assign some boundary points -------------------
    left_cric <- round(qnorm(los/2, mean = mean, sd = sd), 2)
    right_cric <- round(qnorm(1-(los/2), mean = mean, sd = sd), 2)
    t <- round(t, 2)
    p_value <- round(2*pnorm(abs(t), mean = mean, sd = sd, lower.tail = FALSE), 5)
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
    left_cric <- round(qnorm(los, mean = mean, sd = sd), 2)
    t <- round(t, 2)
    p_value <- round(pnorm(t, mean = mean, sd = sd, lower.tail = TRUE), 5)
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
    right_cric <- round(qnorm(1-(los/2), mean = mean, sd = sd), 2)
    t <- round(t, 2)
    p_value <- round(pnorm(t, mean = mean, sd = sd, lower.tail = FALSE), 5)
    # Annotation data -------------------------------
    data_ann <- data.frame(x = c(right_cric, t), y = y_ann,
                           labels = c(paste('Critical point = ', right_cric),
                                      paste('Test statistic = ', t)))
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
  decision <- ifelse(p_value < los, 'Reject ~ H[0]', 'Failed to reject ~ H[0]')
  plot <- plot +
    # manual color filling
    scale_fill_manual(name = 'Region',
                      breaks = c('Rejection Region', 'Acceptance Region', 'P-value'),
                      values = c('red', 'green', 'blue')) +
    # axis title (Labs)
    labs(x = xlab, y = ylab, title = title) +
    # printing lables with the box
    ggtext::geom_richtext(data = data_ann, aes(x = x, y = y, label = labels), angle = 90, size = 3) +
    # annotations
    annotate('text', mean+(3*sd), 0.1, label = paste('P-value', p_value)) +
    annotate('text', mean+(3*sd), 0.08, label = decision, parse = TRUE, col = ifelse(p_value < los, 'red', 'green')) +
    # setting up theme and guide legend
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'top') +
    guides(fill = guide_legend(override.aes = list(alpha = 0.15)))
  return(plot)
}

