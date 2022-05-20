require(BSgenome.Hsapiens.UCSC.hg19)
require(magrittr)
require(stringr)
library('IRanges')
# test overlap2
tab1 <- read.delim("/media/heaven7/Elements/shengxin/java_test/server_ret/out_stage/hotspot_rg.txt",
                   header = F, stringsAsFactors = F)
tab2 <- read.delim("/media/heaven7/Elements/shengxin/java_test/server_ret/out_stage/VEP_rg.txt",
                   header = F, stringsAsFactors = F)
colnames(tab1) <- c("chr", "start", "end")
colnames(tab2) <- c("chr", "start", "end")

grange1 <- GRanges(seqnames = tab1$chr, ranges = IRanges(start = as.integer(tab1$start), 
                                                         end =  as.integer(tab1$end)))
grange2 <- GRanges(seqnames = tab2$chr, ranges = IRanges(start = as.integer(tab2$start), 
                                                         end =  as.integer(tab2$end)))
overRet <- findOverlaps(grange1, grange2) %>% as.matrix
