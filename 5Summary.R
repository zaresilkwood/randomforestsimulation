library(ggplot2)

#Graph for Different Mean
label <- paste0("X", 1:5)
mean  <- seq(0,by = 0.1, length.out = 5) 
lower <- mean - 1
upper <- mean + 1

df <- data.frame(label, mean, lower, upper)

# reverses the factor level ordering for labels after coord_flip()
df$label <- factor(df$label, levels=rev(df$label))

df %>%
  ggplot(aes(x=mean, y=label, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1)+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Mean and Standard Deviation") + ylab("Variable") +
  theme_bw() + # use a white background
  ggsave("graphmean.png", width = 3, height = 2,
       path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

#Graph for different standard deviation
label <- paste0("X", 1:5)
mean  <- rep(0,5) 
lower <- seq(-1, by = -0.1, length.out = 5)
upper <- seq(1, by = 0.1, length.out = 5)

df <- data.frame(label, mean, lower, upper)

# reverses the factor level ordering for labels after coord_flip()
df$label <- factor(df$label, levels=rev(df$label))

df %>%
  ggplot(aes(x=mean, y=label, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1)+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Mean and Standard Deviation") + ylab("Variable") +
  theme_bw() + # use a white background
  ggsave("graphsd.png", width = 3, height = 2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

#Graph for different probability
label <- paste0("X", 1:5)
lower <- rep(0,5)
upper <- seq(0.5, 0.7, length.out = 5)

df <- data.frame(label, lower, upper)

# reverses the factor level ordering for labels after coord_flip()
df$label <- factor(df$label, levels=rev(df$label))

df %>%
  ggplot(aes(x=mean, y=label, xmin=lower, xmax=upper)) +
  geom_errorbarh(height=.1)+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Probability") + ylab("Variable") +
  theme_bw() + # use a white background
  ggsave("graphprob.png", width = 3, height = 2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

#Graph for different number of trial
label <- paste0("X", 1:5)
lower <- rep(0,5)
upper <- seq(1, 5, length.out = 5)

df <- data.frame(label, lower, upper)

# reverses the factor level ordering for labels after coord_flip()
df$label <- factor(df$label, levels=rev(df$label))

df %>%
  ggplot(aes(x=mean, y=label, xmin=lower, xmax=upper)) +
  geom_errorbarh(height=.1)+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Number of Trial") + ylab("Variable") +
  theme_bw() + # use a white background
  ggsave("graphtrial.png", width = 3, height = 2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")

#Graph for different number of category
label <- paste0("X", 1:5)
lower <- rep(1,5)
upper <- seq(2, 6, length.out = 5)

df <- data.frame(label, lower, upper)

# reverses the factor level ordering for labels after coord_flip()
df$label <- factor(df$label, levels=rev(df$label))

df %>%
  ggplot(aes(x=mean, y=label, xmin=lower, xmax=upper)) +
  geom_errorbarh(height=.1)+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Number of Category") + ylab("Variable") +
  theme_bw() + # use a white background
  ggsave("graphcat.png", width = 3, height = 2,
         path = "D:/OneDrive - University of Leeds/@Master Dissertation@/Latex File/image")
