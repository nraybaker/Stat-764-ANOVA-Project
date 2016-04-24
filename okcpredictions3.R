require(tidyr)
require(dplyr)
require(class)
okc.s3 <- as.data.frame(read.csv("okcs1.txt", header = TRUE))
okc.s3 <- subset(okc.s3, select = c("W.L", "TRB", "AST", "STL", "BLK", "OPPTRB", "OPPAST", "OPPSTL", "OPPBLK"))

performance.knn5 <- rep(0, times = 20)
for(i in 1:20){
  set.seed(i); ind <- sample(c(0,1), size = 558, replace = TRUE, prob = c(.25,.75))
  okc.s3_norm <- as.data.frame(lapply(okc.s3[2:9], normalize))
  WL <- okc.s3[1:558, 1]
  okc.s3_norm <- cbind(WL, okc.s3_norm)
  test <- okc.s3_norm[ind == 0, 2:9]
  train <- okc.s3_norm[ind == 1, 2:9]
  cl <- okc.s3_norm[ind == 1, 1]
  test.cl <- okc.s3_norm[ind == 0, 1]
  
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
  okc.s3_norm <- as.data.frame(lapply(okc.s3[2:9], normalize))
  WL <- okc.s3[1:558, 1]
  okc.s3_norm <- cbind(WL, okc.s3_norm)
  test <- okc.s3_norm[ind == 0, 2:9]
  train <- okc.s3_norm[ind == 1, 2:9]
  cl <- okc.s3_norm[ind == 1, 1]
  test.cl <- okc.s3_norm[ind == 0, 1]
  
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
  train <- okc.s3[ind == 1, 1:9]
  test <- okc.s3[ind == 0, 2:9]
  cltest <- okc.s3[ind == 0, 1]
  model.pred3 <- glm(W.L ~ TRB + AST + STL + BLK + OPPTRB + OPPAST + OPPSTL + OPPBLK, data = okc.s3, family = "binomial" )
  prediction <- predict(model.pred3, test, type = "response")
  
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

performance.knn5.2 <- performance.knn5
performance.knn7.2 <- performance.knn7
performance.log.2 <- performance.log
