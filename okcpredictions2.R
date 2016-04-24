require(dplyr)
require(tidyr)
require(class)
okc.s2 <- as.data.frame(read.csv("okcs1.txt", header = TRUE))
okc.s2 <- subset(okc.s2, select = c("W.L", "ORB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "OPPORB", "OPPTRB", "OPPAST", "OPPSTL", "OPPBLK", "OPPTOV", "OPPPF"))

performance.knn5 <- rep(0, times = 20)
for(i in 1:20){
  set.seed(i); ind <- sample(c(0,1), size = 558, replace = TRUE, prob = c(.25,.75))
  okc.s2_norm <- as.data.frame(lapply(okc.s2[2:15], normalize))
  WL <- okc.s2[1:558, 1]
  okc.s2_norm <- cbind(WL, okc.s2_norm)
  test <- okc.s2_norm[ind == 0, 2:15]
  train <- okc.s2_norm[ind == 1, 2:15]
  cl <- okc.s2_norm[ind == 1, 1]
  test.cl <- okc.s2_norm[ind == 0, 1]

  prediction <- knn(train, test, t(cl), k = 5)
  count <- 0
  total <- 0
  for(j in 1:length(test.cl)){
    total <- total + 1
    if(prediction[j] == test.cl[j]){
      count <- count + 1
    }
  }
  performance.knn5[i] <- count/total
}

performance.knn7 <- rep(0, times = 20)
for(i in 1:20){
  set.seed(i); ind <- sample(c(0,1), size = 558, replace = TRUE, prob = c(.25,.75))
  okc.s2_norm <- as.data.frame(lapply(okc.s2[2:15], normalize))
  WL <- okc.s2[1:558, 1]
  okc.s2_norm <- cbind(WL, okc.s2_norm)
  test <- okc.s2_norm[ind == 0, 2:15]
  train <- okc.s2_norm[ind == 1, 2:15]
  cl <- okc.s2_norm[ind == 1, 1]
  test.cl <- okc.s2_norm[ind == 0, 1]
  
  prediction <- knn(train, test, t(cl), k = 7)
  count <- 0
  total <- 0
  for(j in 1:length(test.cl)){
    total <- total + 1
    if(prediction[j] == test.cl[j]){
      count <- count + 1
    }
  }
  performance.knn7[i] <- count/total
}

performance.log <- rep(0, times = 20)
for(k in 1:20){
  set.seed(k); ind <- sample(c(0,1), size = 558, replace = TRUE, prob = c(.25, .75))
  train <- okc.s2[ind == 1, 1:15]
  test <- okc.s2[ind == 0, 2:15]
  cltest <- okc.s2[ind == 0, 1]
  model.pred2 <- glm(W.L ~ ORB + TRB + AST + STL + BLK + TOV + PF + OPPORB + OPPTRB + OPPAST + OPPSTL + OPPBLK + OPPTOV + OPPPF, data = okc.s2, family = "binomial" )
  prediction <- predict(model.pred2, test, type = "response")
  
  for(i in 1:length(prediction)){
    if(prediction[i] < .5){
      prediction[i] <- 0
    }
    else{
      prediction[i] <- 1
    }
  }
  
  count <- 0
  total <- 0
  for(i in 1:length(cltest)){
    total <- total + 1
    if((prediction[i] == 1) && (cltest[i] == "W")){
      count <- count + 1
    }
    else if((prediction[i] == 0) && (cltest[i] == "L")){
      count <- count + 1
    }
  }
  performance.log[k] <- count/total
}

performance.knn5.1 <- performance.knn5
performance.knn7.1 <- performance.knn7
performance.log.1 <- performance.log
