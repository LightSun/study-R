system("echo anno1 done.")
chr <- c("X", "Y", "Z", "chrY", "Y")
# replace the chr-ele which equals ele-in-c to 'Ymt'
chr[chr %in% c("Y", "MT", "chrY", "chrMT")] <- "Ymt"
# remove duplicate
chr <- unique(chr)


## %>% indicate: pip
## f(x)=sin((x+1)^2)
f1 <- function(x){return(x+1)}
f2 <- function(x){return(x^2)}
f3 <- function(x){return(sin(x))}
a <- 1
b <- a %>% f1 %>% f2 %>% f3
print(b)

# which函数
# 1. 返回满足条件的向量下标
cy <- c(1,4,3,7,NA)
cy
which(cy>3)   #返回下标(may be array)
cy[which(cy>3)]   #返回下标对应的值


## do.call
## 3 count of 5 * 5
## 1~5  step is 5
## 4-8  step is 5
## 21-25 step is 5
dat <- list(matrix(1:25, ncol = 5), matrix(4:28, ncol = 5), matrix(21:45, ncol=5))
# 5 rows 15 cols
dat_bindc <- do.call(cbind,dat)
# 15 rows 5 cols. 竖直方向叠加（列数不变，行叠加）
dat_bindr <- do.call(rbind,dat)

##
pr <- paste(rep("sub", 5), collapse = ",")
evalRet <- eval(parse(text = paste0("rbind(", pr,")")))


## colnames/rownames
'使用函数colnames ()和rownames ()分别给矩阵的列和行命名。 
可以使用与向量索引同样的方法访问数组中的元素。 R语言 数据框常用 colnames rename来更改变量名
colnames 主要是对矩阵或数据框进行列命名，类似的还有rownames.'

## data.frame. R语言用来处理表格式数据的数据结构。
team_name <- c("Chicago Bulls", "Golden State Warriors")
wins = c(72, 73)
losses = c(10, 9)
is_champion = c(TRUE, FALSE)
season = c("1995-96", "2015-16")
great_nba_teams = data.frame(team_name, wins, losses, is_champion, season)

chacheDir <- "/media/heaven7/h7/shengxin/vepchache";
load(paste0(chacheDir, "/Data/annotation.pre.v1.Rdata"))

# test read.delim(...)[,1]
chacheDir <- "/media/heaven7/h7/shengxin/vepchache";
# only have one col. no head tag
clinvarTrans0 <- read.delim(paste0(chacheDir, "/Data/clinvar/clinvar_transcript_hg38.txt"), 
                           header = F, stringsAsFactors = F)
# get the data as sequence/array.
clinvarTrans <- clinvarTrans0[,1] # the col = 1 data.
domainHot <- read.delim(paste0(chacheDir, 
                               "/Data/interpro/interpro.domainRegion_hot.hg19.txt"), 
                        header = T, stringsAsFactors = F)

## IRanges/GRanges as multi intervals
## include start, end, width. [name]
library('IRanges')
ir1 <- IRanges(start = c(1,3,5), end = c(3,5,7)) 
ir1
## IRanges of length 3 
## start end width 
## [1] 1 3 3 
## [2] 3 5 3 
## [3] 5 7 3

# add name for interval
names(ir1) <- paste("A", 1:3, sep = "") 
ir1

## IRanges of length 3 
## start end width names 
## [1] 1 3 3 A1 
## [2] 3 5 3 A2 
## [3] 5 7 3 A3

# get the first line
ir1[1] ## [1] 1 3 3 A1
# get the line of target interval name
ir1["A1"] ## ## [1] 1 3 3 A1
# merge two IRanges. in lines
c(ir1, ir2)


# test arr
arr = array(1:20, dim = c(4, 5))
# 取第3列数据
arr2 = arr[,3]
# 取第1-4列数据
arr3 = arr[,1:4]
# 取第1行的数据
arr4 = arr[1,]
#arr_2 = array(21:40, dim = c(4, 5))
#narr = c(arr, arr_2)
#x <- narr[[2]] # a digital value

idx <- which(arr > 5)

# eq  if. else
sum0 <- sum(!"2" %in% "-") # 1
sum1 <- sum(!"-" %in% "-") # 0

#diff . the diff with two elements
test <- sample(1:10,5) # like '3, 5, 10, 8, 2'
a <- diff(test) # like '2, 5, -2, -6'

# vep
rep_res <- rep(0, 5); # '0, 0, 0 ,0, 0'

#Na
a1 <- NA
a2 <- a1 > 0 #Na
a3 <- a2 <= 0 # Na

#
BP4.revel <- rep(0, 5) 
BP4.revel[1] <- 1
REVEL.b <- rep(FALSE, 5)
REVEL.b[2] <- TRUE
BP4.revel[REVEL.b] <- 1
BP4.revel # 1 1 0 0 0

# which.  match and return index
which_res = which(BP4.revel == 1)
chas = nchar("123")
#
groupMat <- data.frame(or = 1:length(BP4.revel), 
                       group = rep(1:3, each=2)[1:length(BP4.revel)],  #1,1,2,2,3
                       stringsAsFactors = F)

##
Bsmat <- c(0,1,0,1,1)
BP4.revel <- rep(0, 5)
BP4.revel[Bsmat] <- 1
BP4.revel

## match
mr <- match(c(1, "TRUE", "A", "1"), c(T, 0, "1", "1"))
mrd <- data.frame(gender=c("M","M","F","M","F","F"),age=c(47,59,21,32,33,24),
                income=c(55000,88000,32450,76500,123000,45650))
#mrdd <- mrd[mr, c("age", "bsd")] # column must exist
mrdt <- mrd$age[mr]
mrdt2 <- mrd[mr, c("age", "gender")]

## split
mrdd <- split(mrd[,c("gender","age")], c("a","b","c","d","e","c"))
mrdd
mrdd_len <- length(mrdd)

## strsplit
require(magrittr)
zstr <- "1|2|3|4|5|6|7"
zstr1 <- strsplit(zstr, "\\|")
zstr2 <- zstr1[[1]][3:6] 
zstr2
zstr3 <- zstr2 %>% as.numeric %>% max
zstr3

x <- "1,2,3,4,5,6,7"
xr <- strsplit(as.character(x), ",")
xr

## 
xr2 <- c("1,2,3,4,5,6,7", "a,b,c")
xrt2 <- do.call(cbind,lapply(strsplit(as.character(xr2), ","), function(mm){
  mm
}))
xrt2
xrt3s <- data.frame(gender=c("1,2,3,4,5,6,7", "a,b,c"))
xrt4 <- strsplit(as.character(xrt3s$gender), ",")
xrt4
xrt3 <- do.call(cbind,lapply(strsplit(as.character(xrt3s$gender), ","), function(mm){
  mm
}))
xrt3

## grepl
grt <- grepl(",", "a,b")
grt1 <- grepl("frameshift|stop_gain", "stop_gain1")
grt11 <- grepl("frameshift|stop_gain", "1frameshift1")
grt2 <- grepl("a|b", "b")

## isNa
na1 = is.na(c(1,NA))
na1
na2 = is.na(c(NA,NA))
na2
if(is.na(na1)){
  print("is.na(na1) = true")
}else{
  print("is.na(na1) = false")
}
## unique
ur <- data.frame(gender=c("M","F","M"),age=c(47,47,47))
ur2 <- unique(ur)
ur <- data.frame(gender=c("M","F","M"),age=c(47,48,49))
ur2 <- unique(ur$gender)
ur2
## table. count the element appear count
z <- c(1,2,2,4,2,7,1);
z1 <-table(z);
z1
zt <- names(which(table(z) >= 2))

## collapse
zrt <- paste0(z, collapse = "+")

## sub
sub1 <- sub("(NM_[0-9]+).*","\\1","NM_005101.4")
sub2 <- sub("(NM_[0-9]+).*","\\1","NC_000016.10")

# gsub
ret <- gsub("\\.[0-9]", "*", "abc.12.3l")
cs = order(c("X","Y","M"))

# rep
rep1 <- rep(c(0, 1), 3)
grt <- grepl("a", "a")
grt2 <- grepl("a", "ab")

# strsplit
require(BSgenome.Hsapiens.UCSC.hg19)
require(magrittr)
require(stringr)
cling <- c("a", "a&b", "a&b&c", "a&b&c&d")
clingr <- strsplit(cling, "&")
clingr1 <- do.call(rbind, clingr) %>% data.frame(.,stringsAsFactors = F)

# parLapply
num <- c(1:3)
cl <- makeCluster(4) # 初始化四核心集群
square <- function(x){
   c(x,x + 1)
}
results <- parLapply(cl, num, square)#调用parLapply并行计算平方函数
final <- do.call('c',results)#整合结果
stopCluster(cl) # 关闭集群
#parApply  行数不变，追加在列后面


tab_name <- read.delim("/home/heaven7/heaven7/study/github/mine/Shengxin/task_performance/name.txt",
                           header = T, stringsAsFactors = F)
tab_tid <- read.delim("/home/heaven7/heaven7/study/github/mine/Shengxin/task_performance/tid.txt",
                   header = T, stringsAsFactors = F)
tab_temp = tab_name[match(tab_tid$tid, tab_name$id), c("id", "name")]
write.table(tab_temp, file = paste0("/home/heaven7/heaven7/study/github/mine/Shengxin/task_performance/test.txt"), 
            col.names = T, row.names = F, sep = "\t", quote = F)

tab_unique <- read.delim("/home/heaven7/heaven7/study/github/mine/Shengxin/task_performance/test_unique.txt",
                       header = T, stringsAsFactors = F)
undup.VEP0 <- unique(tab_unique[, c("X.Uploaded_variation", "CLIN_SIG", "MAX_AF")])

tab_strs <- read.delim("/home/heaven7/heaven7/study/github/mine/Shengxin/task_performance/strsplit.txt",
                       header = T, stringsAsFactors = F)
tab_clinvar <- do.call(rbind, strsplit(tab_strs$clinvar, "&")) %>% data.frame(.,stringsAsFactors = F)
tab_clinvar2 <- do.call(rbind, strsplit(tab_strs$clinvar, "&"))
tab_clinvar3 <- do.call(cbind, strsplit(tab_strs$clinvar, "&"))
tab_clinvar4 <- tab_clinvar3[,1:2]
tab_clinvar5 <- match(tab_strs$id, "1")
tab_strs$POS
orret <- order(tab_clinvar5, tab_strs$POS)
orret
orret2 <- order(tab_strs$id, tab_strs$POS)
orret2
orret3 <- order(tab_strs$id)
orret3
tab_clinvar51 <- tab_strs[orret,]
#Refseq_main <- which(colnames(VEPres) %in% "Refseq_main")
# test 

#test
tab1 <- read.delim("/home/heaven7/heaven7/study/github/mine/Shengxin/task_performance/granges.txt",
                       header = T, stringsAsFactors = F)
tab2 <- read.delim("/home/heaven7/heaven7/study/github/mine/Shengxin/task_performance/granges2.txt",
                   header = T, stringsAsFactors = F)
grange1 <- GRanges(seqnames = tab1$chr, ranges = IRanges(start = as.integer(tab1$start), 
                                                          end =  as.integer(tab1$end)))
grange2 <- GRanges(seqnames = tab2$chr, ranges = IRanges(start = as.integer(tab2$start), 
                                                         end =  as.integer(tab2$end)))
overRet <- findOverlaps(grange1, grange2) %>% as.matrix #%>% as.matrix

d <- data.frame(gender=c("M","M","F","M","F","F"),age=c(47,59,21,32,33,24),
                income=c(55000,88000,32450,76500,123000,45650), 
                over25=rep(c(1,1,0), times=2))
dr <- paste0(d$gender, "_", d$over25)
dr1 <- dr[d$gender %in% "M"]
# group by combinedColumns.
dt <- split(d$income, list(d$gender,d$over25))
# only get the group result.length = 1.
dt.ID <- names(dt)[sapply(dt, length) == 1] # 

