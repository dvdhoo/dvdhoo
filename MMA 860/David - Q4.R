data <- read_excel("~/OneDrive - Queen's University/MMA/MMA 860/Assignment 1/MMA860_Assignment1_Data_vf.xlsx",
                   sheet = "Collinearity")

data <- read_excel(file.choose(),
                   sheet = "Collinearity")


#Q4

#a

data2 <- filter(data, data$Obs <= 25)

data2_graph1 <- ggplot(data = data2, aes(y = Y, x = Experience))
data2_graph1 + geom_point(aes(shape = 18)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_identity()

data2_graph2 <- ggplot(data = data2, aes(y = Y, x = Height))
data2_graph2 + geom_point(aes(shape = 18)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_identity()

data2_graph3 <- ggplot(data = data2, aes(y = Y, x = Weight))
data2_graph3 + geom_point(aes(shape = 18)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_identity()

data_graph1 <- ggplot(data = data, aes(y = Y, x = Experience))
data_graph1 + geom_point(aes(shape = 18)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_identity()

data_graph2 <- ggplot(data = data, aes(y = Y, x = Height))
data_graph2 + geom_point(aes(shape = 18)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_identity()

data_graph3 <- ggplot(data = data, aes(y = Y, x = Weight))
data_graph3 + geom_point(aes(shape = 18)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_identity()



  #i
data2_reg <- lm(data = data2, formula = Y ~ Experience + Height)
data_reg <- lm(data = data, formula = Y ~ Experience + Height)

summary(data2_reg)
summary(data_reg)

  #ii
data2_reg2 <- lm(data = data2, formula = Y ~ Experience + Weight)
data_reg2 <- lm(data = data, formula = Y ~ Experience + Weight)

summary(data2_reg2)
summary(data_reg2)

  #iii
data2_reg3 <- lm(data = data2, formula = Y ~ Experience + Weight + Height)
data_reg3 <- lm(data = data, formula = Y ~ Experience + Weight + Height)

summary(data2_reg3)
summary(data_reg3)



#b
#data2_reg4 <- lm(data = data2, formula = Y ~ Weight)
data_reg4 <- lm(data = data, formula = Y ~ Weight)

#summary(data2_reg4)
summary(data_reg4)

#data2_reg5 <- lm(data = data2, formula = Y ~ Height)
data_reg5 <- lm(data = data, formula = Y ~ Height)

#summary(data2_reg5)
summary(data_reg5)

#data2_reg6 <- lm(data = data2, formula = Y ~ Experience)
data_reg6 <- lm(data = data, formula = Y ~ Experience)

#summary(data2_reg6)
summary(data_reg6)

#height/weight correlation
cor(data$Height,data$Weight)


