if(!require('tidyverse')) {
  +     install.packages('tidyverse')
  +     library('tidyverse')}
 
if(!require('HSAUR2')) {
    +     install.packages('HSAUR2')
    +     library('HSAUR2')}
data("skulls") # use this data set for proceeding code
names(skulls) # get the variables
summary(skulls) # summary of the data
means_by_epoch <- skulls %>%
  +     group_by(epoch) %>%
  +     summarise(across(where(is.numeric), mean, na.rm = TRUE))
means_long <- means_by_epoch %>%
  +     pivot_longer(cols = -epoch, names_to = "measurement", values_to = "mean_value")
ggplot(means_long, aes(x = epoch, y = mean_value, fill = measurement)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Values of Skull Measurements by Epoch",
       x = "Epoch",
       y = "Mean Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# Add manova code below
##################################

# TODO
skulls.manova1 <- 
  manova(cbind(mb, bh, bl, nh) ~ as.factor(epoch),
         data = skulls)

# prepare different types of manova tests
summary(skulls.manova1, test == "Hotelling-Lawley")
summary(skulls.manova1, test == "Roy")
summary(skulls.manova1, test == "Pilai")
summary(skulls.manova1, test == "Wilks")

summary.aov(skulls.manova1)
cat("summary of aov")

skulls.manova2 <- 
  manova(cbind(mb, bh, bl, nh) ~ as.factor(epoch),
         data = skulls,
         subset = as.factor(epoch) %in% c("c4000BC", "c200BC")
  )
summary(skulls.manova2)                                           
                                            