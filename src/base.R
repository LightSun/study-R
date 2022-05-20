# see the loaded packages
(.packages())

# unload package
#detach(package:RMySQL)

# delete package
#remove.packages("RMySQL")

#update package
#update.packages()

#
library(ggplot2)
print(mpg)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# Listing 1.1 - A Sample R session
age <- c(1,3,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
mean(weight)
sd(weight) # stand deviation
cor(age,weight) #月龄
plot(age,weight)
# q()

# paste 默认以空格作为连接字符, paste0 以空字符串连接字符. 
paste("a", "b", "c") # "a b c"
paste0("a", "b", "c")
# chr1 -> chr22, chrX, chrY, chrM 
chr <- c(paste("chr", 1:22, sep = ""), "chrX", "chrY", "chrM")

#设置核心数：no_cores <- detectCores() - 1
#步骤分群环境：cl <- makeCluster(no_cores)
#用到的变量与包复制给不同的核心：clusterEvalQ（包）、clusterExport（变量）
#运行算法：clusterApply(cl, c(9,5), get("+"), 3) 
#关闭集群：stopCluster(cl)
#
# more: https://www.cnblogs.com/payton/articles/6113008.html

library(parallel)
cl <- makeCluster(25) # multi thread

