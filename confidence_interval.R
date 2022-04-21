sample <- c(62, 63, 64, 65, 67, 67, 68, 69, 70, 72)
# Sample size
n <- length(sample)
# Sample standard deviation
s <- sqrt(sum((sample - 66)^2)/(n-1))
# Population assumed standard deviation
sigma_0 <- 2
# Test statistic
t_calculated <- (n*(s^2))/(sigma_0^2)
t_calculated

shaded_region <- function(df, start, end, fill, alpha){
  geom_area(data = subset(df, x > start & x < end),
            aes(y = prob), fill = fill, color = NA, alpha = alpha)
}

library(tidyverse)
data <- data.frame(x = seq(0,30,0.01), prob = dchisq(seq(0,30,0.01), df = 10))
left_cric <- qchisq(0.025,10)
right_cric <- qchisq(0.975,10)
df <- 10
data %>% 
  ggplot(aes(x = x, y = prob)) +
  geom_line() +
  geom_vline(aes(xintercept = qchisq(0.025,10)), col = 'red', size = 0.8, alpha = 0.2) +
  shaded_region(data, 0, left_cric, 'red', 0.2) +
  shaded_region(data, right_cric, 30, 'red', 0.2) +
  shaded_region(data, t_calculated, 30, 'blue', 0.2) +
  geom_vline(aes(xintercept = left_cric), col = 'red', size = 0.8, alpha = 0.2, linetype = 'dashed') +
  geom_vline(aes(xintercept = t_calculated), col = 'blue', size = 0.8, alpha = 0.4) +
  labs(x = 'x', y = 'Probability', title = paste('Chi-square distribution with df = ', df)) +
  theme(title = element_text(hjust = 0.5))

make_chi_square_dist <- function(t, df, ltail, rtail, los){
  # t: Value of the test statistic
  # df: Degree of freedom
  # ltail: left point of the random variable
  # rtail: Right point of the random variable
  # los: Level of significance
  
  # Making synthetic data -------------------------
  data <- data.frame(x = seq(0,rtail,0.01), prob = dchisq(seq(0,rtail,0.01), df = df))
  # Assign some boundary points -------------------
  left_cric <- round(qchisq(los/2, df), 2)
  right_cric <- round(qchisq(1-(los/2), df), 2)
  t <- round(t, 2)
  p_value <- round(pchisq(t, df = df, lower.tail = FALSE), 5)
  decision <- ifelse(p_value < los, 'Reject ~ H[0]', 'Failed to reject ~ H[0]')
  # Annotation data -------------------------------
  data_ann <- data.frame(x = c(left_cric, right_cric, t), y = 0.075,
                         labels = c(paste('Left Critical point = ', left_cric), 
                                    paste('Right Critical point = ', right_cric),
                                    paste('Test statistic = ', t)))
  # Shaded region function ------------------------
  shaded_region <- function(df, start, end, fill, alpha){
    geom_area(data = subset(df, x > start & x < end),
              aes(y = prob, fill = fill), color = NA, alpha = alpha)
  }
  # Plotting --------------------------------------
  plot <- data %>% 
    ggplot(aes(x = x, y = prob)) +
    geom_line() +
    geom_vline(aes(xintercept = left_cric), col = 'red', size = 0.8, alpha = 0.2, linetype = 'dashed') +
    geom_vline(aes(xintercept = right_cric), col = 'red', size = 0.8, alpha = 0.2, linetype = 'dashed') +
    geom_vline(aes(xintercept = t), col = 'blue', size = 0.8, alpha = 0.2, linetype = 'dashed') +
    shaded_region(data, ltail, left_cric, 'Rejection Region', 0.2) +
    shaded_region(data, right_cric, rtail, 'Rejection Region', 0.2) +
    shaded_region(data, left_cric, right_cric, 'Acceptance Region', 0.2) +
    shaded_region(data, t, rtail, 'P-value', 0.5) +
    scale_fill_manual(name = 'Region',
                      breaks = c('Rejection Region', 'Acceptance Region', 'P-value'),
                      values = c('red', 'green', 'blue')) + 
    labs(x = 'x', y = 'Probability',
         title = paste('Chi-square distribution with df = ', df)) +
    ggtext::geom_richtext(data = data_ann, aes(x = x, y = y, label = labels), angle = 90, size = 3) +
    annotate('text', 30, 0.013, label = paste('P-value', p_value)) +
    annotate('text', 30, 0.009, label = decision, parse = TRUE, col = ifelse(p_value < los, 'red', 'green')) + 
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = 'top') +
    guides(fill = guide_legend(override.aes = list(alpha = 0.15)))
  return(plot)
}

plot1 <- make_chi_square_dist(26.94444, 10, 0, 35, 0.05)
ggsave('./STAT-2/problem_1a.png', plot1,  'png', height = 9, width = 16, dpi = 300)

plot2 <- make_chi_square_dist(23.025, 9, 0, 35, 0.05)
ggsave('./STAT-2/problem_1b.png', plot2,  'png', height = 9, width = 16, dpi = 300)


plot3 <- make_chi_square_dist(57.33609, 39, 10, 75, 0.05)
plot3
ggsave('./STAT-2/problem_1b.png', plot3,  'png', height = 9, width = 16, dpi = 300)



# Function for both tail normal distribution ------------------------------

make_normal_dist <- function(t, mean, sd, los, y_ann){
  # t: Value of the test statistic
  # mean: Mean of the distribution
  # sd: Standard deviation of the distribution
  # ltail: left point of the random variable
  # rtail: Right point of the random variable
  # los: Level of significance
  ltail <- mean - (3.5*sd)
  rtail <- mean + (3.5*sd)
  # Making synthetic data -------------------------
  data <- data.frame(x = seq(ltail,rtail,0.01), 
                     prob = dnorm(seq(ltail,rtail,0.01), mean = mean, sd = sd))
  # Assign some boundary points -------------------
  left_cric <- round(qnorm(los/2, mean = mean, sd = sd), 2)
  right_cric <- round(qnorm(1-(los/2), mean = mean, sd = sd), 2)
  t <- round(t, 2)
  p_value <- round(2*pnorm(abs(t), mean = mean, sd = sd, lower.tail = FALSE), 5)
  decision <- ifelse(p_value < los, 'Reject ~ H[0]', 'Failed to reject ~ H[0]')
  # Annotation data -------------------------------
  data_ann <- data.frame(x = c(left_cric, right_cric, t), y = y_ann,
                         labels = c(paste('Left Critical point = ', left_cric), 
                                    paste('Right Critical point = ', right_cric),
                                    paste('Test statistic = ', t)))
  # Shaded region function ------------------------
  shaded_region <- function(df, start, end, fill, alpha){
    geom_area(data = subset(df, x > start & x < end),
              aes(y = prob, fill = fill), color = NA, alpha = alpha)
  }
  # Plotting --------------------------------------
  plot <- data %>% 
    ggplot(aes(x = x, y = prob)) +
    # density line
    geom_line() +
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
    shaded_region(data, abs(t), rtail, 'P-value', 0.5) +
    # manual color filling
    scale_fill_manual(name = 'Region',
                      breaks = c('Rejection Region', 'Acceptance Region', 'P-value'),
                      values = c('red', 'green', 'blue')) +
    # axis title (Labs)
    labs(x = 'x', y = 'Probability',
         title = paste('Normal distribution with mean = ', mean, '& sd = ', sd)) +
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

plot4 <- make_normal_dist(-2.25, 0, 1, 0.05, 0.3)
ggsave('./STAT-2/problem_3.png', plot4,  'png', height = 9, width = 16, dpi = 300)
