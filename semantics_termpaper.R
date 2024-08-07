# 環境設定
packages <- c("tidyverse", "ggplot2", "dplyr", "readxl", "tidytext", "writexl", "Rling", "cluster", "pvclust", "vcd", "ggrepel")
lapply(X = packages, FUN = library, character.only = T)

#### 爆 ####
# bao.tb <- read_excel("./語料/term_paper_linguistic_data/ws_output/ptt_爆.xlsx")
# bao.data <- read_excel("./語料/term_paper_linguistic_data/output/分析資料_爆.xlsx")

bao.tb %>% 
    select(title, para_ckip, word_pos) %>%
    drop_na() %>%
    mutate(title_id = row_number(), .before = title) -> bao.tb

bao.tb %>%
    select(-para_ckip) -> bao.tb


bao.tb %>%
    filter(str_detect(word_pos, ".*爆/[A-z]+")) -> bao.target.tb

bao.target.tb %>%
    filter(str_detect(word_pos, ".*/[A-z]+\\s爆/[A-z]+\\s.*/[A-z]+"))

# 找出前面的字
bao.target.tb %>%
    unnest_tokens(
        input = word_pos,
        output = bao_pattern,
        token = function(x)
            str_extract_all(x, "\\s.*/[A-z]+\\s爆/[A-z]+\\s.*/[A-z]+\\s")
    )

## 爆的前幾名lemma
bao.tb %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    filter(str_detect(word, "爆/vh")) %>%
    count(word) %>%
    arrange(desc(n))

## 取出爆、驚爆、遭爆
bao.tb %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    filter(str_detect(word, "爆/vh|驚爆/vh|遭爆/vh")) %>%
    separate(col = word, into = c("word", "pos"), sep = "/") %>%
    filter(str_detect(word, pattern = "\\b爆\\b|驚爆|狂爆|遭爆|頻爆")) -> bao.target.result

## 取出被/p爆/vh, 遭/P爆/VH
bao.tb %>%
    filter(str_detect(word_pos, "[被|遭|再|連環|又]/[A-z]+ 爆/VH")) %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    separate(col = word, into = c("word", "pos"), sep = "/") %>%
    filter(str_detect(word, pattern = "被|遭|再|連環|又")) -> bao.front.target.result

## 只選title，合併兩個tb，隨機選擇1000筆資料
bao.target.result %>%
    select(title) -> bao1

bao.front.target.result %>%
    select(title) -> bao2

rbind(bao1, bao2) %>%
    distinct() %>%
    mutate(title_id = row_number(), .before = title) -> bao3

set.seed(123)
bao3_result <- bao3[sample(x = nrow(bao3), size = 1000),]
# write_xlsx(bao3_result, path = "./語料/term_paper_linguistic_data/output/分析資料_爆.xlsx")

## 爆標題重複性檢查
bao_not_used_title <- setdiff(bao3$title,bao.data$title)
set.seed(123)
bao_used_title <- c()
pu.data %>%
    filter(str_detect(title, "爆")) %>%
    filter(!str_detect(title, "爆炸")) %>%
    filter(!str_detect(title, "爆發")) %>%
    filter(!str_detect(title, "爆打|引爆|爆出|推爆|射爆|氣爆|摳爆")) -> pu.bao.tb
bao_not_used_title3 <- setdiff(pu.bao.tb$title, bao_used_title)


bao_new_title <- sample(bao_not_used_title3, size = 1, replace = F)
bao_new_title %in% bao3_result$title
bao_new_title %in% bao_used_title
bao_new_title
bao_used_title <- c(bao_used_title, bao_new_title)

## 檢驗爆的前後字
# bao pattern
## 製作bao_bigram
bao.tb %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    select(word) %>%
    as.list() %>%
    as.tokens() %>%
    tokens_ngrams(n = 2, skip = 0, concatenator = "_") %>%
    as.list() -> bao.bigram

## 找bigram的前面的一個字
bao.bigram %>%
    data.frame() %>%
    filter(str_detect(word, pattern = ".+/[A-z]+_爆/vh")) %>%
    separate(col = word, into = c("front", "target"), sep = "_") %>%
    separate(col = front, into = c("front", "front_pos"), sep = "/") -> bao.front.bigrams
## 爆前面的字的前幾名
bao.front.bigrams %>%
    count(front) %>%
    arrange(desc(n))
## 爆前面的pos的前幾名
bao.front.bigrams %>%
    count(front_pos) %>%
    arrange(desc(n))

## 找bigram的後面的一個字
bao.bigram %>%
    data.frame() %>%
    filter(str_detect(word, pattern = "爆/vh_.+/[A-z]+")) %>%
    separate(col = word, into = c("target", 'back'), sep = "_") %>%
    separate(col = back, into = c("back", "back_pos"), sep = "/") -> bao.back.bigrams

## 爆後面的字的前幾名
bao.back.bigrams %>%
    count(back) %>%
    arrange(desc(n))
## 爆後面的pos的前幾名
bao.back.bigrams %>%
    count(back_pos) %>%
    arrange(desc(n))

# 搜尋範例
# str_match(bao.tb$title[1:10], "\\[[\u4e00-\u9fa5]+\\]")
# str_match(bao.tb$title[1:20], "\\[.*\\]")


#### 爆 計算 ####
# bao.data %>%
#     filter(str_detect(title, pattern = "^再爆"))
bao.data %>%
    mutate(PREDPREP = ifelse(VOICE == "passive" & TRANSITIVITY == "transitive" & PREDPREP == "NONE", "Bèi-less", PREDPREP),
           .before = SENT_TYPE) -> bao.data
# write_xlsx(bao.data, path = "./語料/term_paper_linguistic_data/output/最終分析資料_爆.xlsx")

## bao concordance ####
# bao.tb %>%
#     unnest_tokens(
#         input = word_pos,
#         output = word,
#         token = function(x)
#             str_split(x, pattern = " ")
#     ) -> bao.word.tb
# 
# as.tokens(list(bao.word.tb$word)) -> bao.tokens
# kwic(bao.tokens, "爆/vh") -> bao.concordance
# write_xlsx(bao.concordance, path = "./semantics_term_paper_topics_data/term_paper_linguistic_data/output/爆concordance.xlsx")

## bao counts => 目標是爆/vh => 接下來找出爆/vh的上下文環境
# bao.word.tb %>%
#     select(word) %>%
#     filter(str_detect(word, pattern = "爆.*")) %>%
#     count(word) %>%
#     arrange(desc(n)) -> bao.counts
# write_xlsx(bao.counts.tb, path = "semantics_term_paper_topics_data/爆詞數量.xlsx")

# 清除空白與標點符號
# bao.word.tb %>%
#     filter(nzchar(word)) -> bao.word.tb2
    # filter(!str_detect(word, "[^\u4E00-\u9FFF_]")) -> bao.word.tb2

# 算bigram
# bao.word.tb2 %>%
#     select(word) %>%
#     as.list() %>%
#     as.tokens() %>%
#     tokens_ngrams(n=2, skip=0, concatenator = "_") %>%
#     as.list() -> bao.pos.bigram
# 
# bao.pos.bigram %>%
#     data.frame() %>%
#     count(word) %>%
#     filter(str_detect(word, "爆/vh")) %>%
#     arrange(desc(n)) -> bao.pos.bigrams
# write_xlsx(bao.bigram.counts, path = "semantics_term_paper_topics_data/爆bigram數量.xlsx")

# bao.counts.tb %>%
#     filter(n >= 30) %>%
#     wordcloud2()

#### 曝 ####
pu.tb <- read_xlsx("./語料/term_paper_linguistic_data/ws_output/ptt_曝.xlsx")
pu.data <- read_xlsx("./語料/term_paper_linguistic_data/output/分析資料_曝.xlsx")

pu.data %>%
    mutate(TRANSITIVITY = ifelse(VOICE == "passive", "intransitive", "transitive")) -> pu.data


pu.tb %>% 
    select(title, para_ckip, word_pos) %>%
    drop_na() %>%
    mutate(title_id = row_number(), .before = title) %>%
    select(-para_ckip) -> pu.tb

pu.tb %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    select(word) %>%
    as.list() %>%
    as.tokens() %>%
    tokens_ngrams(n = 2, skip = 0, concatenator = "_") %>%
    as.list() -> pu.bigram

## 曝的前幾名lemma
pu.tb %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    filter(str_detect(word, "曝/vc")) %>%
    count(word) %>%
    arrange(desc(n)) 

## 找bigram的前面的一個字
pu.bigram %>%
    data.frame() %>%
    filter(str_detect(word, pattern = ".+/[A-z]+_曝/vc")) %>%
    separate(col = word, into = c("front", "target"), sep = "_") %>%
    separate(col = front, into = c("front", "front_pos"), sep = "/") -> pu.front.bigrams

## 曝前面的字的前幾名
pu.front.bigrams %>%
    count(front) %>%
    arrange(desc(n)) %>%
    head(30)
## 曝前面的pos的前幾名
pu.front.bigrams %>%
    count(front_pos) %>%
    arrange(desc(n))
## 
## 取出曝、驚曝、遭曝
pu.tb %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    filter(str_detect(word, "\\b曝\\b/vc|驚曝/vc|遭曝/vc")) %>%
    separate(col = word, into = c("word", "pos"), sep = "/") -> pu.target.result

## 取出曝前面的一些字
pu.tb %>%
    filter(str_detect(word_pos, "[再|自|被|」| ]/[A-z]+ 曝/VC")) %>%
    unnest_tokens(
        input = word_pos,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) %>%
    separate(col = word, into = c("word", "pos"), sep = "/") %>%
    filter(str_detect(word, pattern = "再|自|被|」| ")) -> pu.front.target.result

## 只選title，合併兩個tb，隨機選擇1000筆資料
pu.target.result %>%
    select(title) -> pu1

pu.front.target.result %>%
    select(title) -> pu2

rbind(pu1, pu2) %>%
    distinct() %>%
    mutate(title_id = row_number(), .before = title) -> pu3

set.seed(123)
pu3_result <- pu3[sample(x = nrow(pu3), size = 1000),]
# write_xlsx(pu.data, path = "./語料/term_paper_linguistic_data/output/分析資料_曝.xlsx")


pu.data %>%
    mutate(PREDPREP = ifelse(VOICE=="passive" & TRANSITIVITY == "intransitive" & PREDPREP == "NONE" & COMPLEMENT == "NONE", "Bèi-less", PREDPREP)) -> pu.data

# write_xlsx(pu.data, path = "./語料/term_paper_linguistic_data/output/最終分析資料_曝.xlsx")


## 替代曝光的title
# replace_pu_num <- c(1069, 1649, 193, 1444, 1126,
#                     283, 373, 1748, 37, 959,
#                     1884, 1173, 386, 1597, 1101,
#                     137, 804, 329, 1261, 136,
#                     1796, 1362, 1569, 354, 719,
#                     188, 504, 1354, 568, 1292,
#                     1143, 232, 1427, 1563, 245,
#                     1552, 327, 1288, 887, 817,
#                     1543, 1690, 4, 1819, 71,
#                     1482, 978, 1301, 1661, 9,
#                     1330, 1492, 772, 1509, 1816,
#                     75, 1550, 1682, 1167, 150,
#                     1355, 135)
# 
# pu3 %>%
#     filter(!endsWith(title, "曝")) -> pu3_1 # 去除曝結尾的標題

# 有空再想如何自動化這段過程，目前無法想出
set.seed(123)
# used_title <- c("傳林智堅碩論抄襲比例低　台大教授曝「",
#                 "驚險影像曝！表哥不給搭便車！他趴引擎蓋",
#                 "七星連珠來襲！命理師曝「吉凶運勢」　投資務必謹慎小心",
#                 "柯王CP真愛破局！柯文哲曝「禮物收回」",
#                 "鬼月來了　印度神童師姐曝最新禁忌　有祂在不怕殭屍與鬼",
#                 "「我從事的是理念和想法」 于北辰曝與吳",
#                 "網文曝中共稅收「腰斬」背後 居民收入暴",
#                 "「Freddy安啦」　他曝林昶佐不會被罷免",
#                 "徐耀昌曝林智堅曾來電分割苗栗 「頭份給",
#                 "國民黨曝林靜儀拍桌影片 疑耍官威「你們",
#                 "雨中偶遇小奶貓 他曝「10年對比圖」惹哭",
#                 "拜登GG？馬斯克曝不再支持民主黨 將轉投",
#                 "歐陽娜娜曝「姊姊從不會說想我」 哽咽落",
#                 "中國槍手「仇恨台人」原因曝！曾住台灣",
#                 "不是中間選民！王浩宇曝「1年齡層」流失",
#                 "獨家／納豆腦出血24天最新病況曝！爆「無",
#                 "陳時中新書曝推崇共黨林彪帶兵心法　防",
#                 "Omicron攻破桃機　國民黨曝「關鍵這天」",
#                 "嘆顏寬恒也受一樣待遇 陳佩琪曝柯文哲感"
#                 )

# non_used_title <- setdiff(pu3_1$title, pu.data$title)
second_non_used_title <- setdiff(non_used_title, used_title)
# new_title <- sample(non_used_title, size = 1)
new_title <- sample(second_non_used_title, size = 1)
new_title %in% pu.data$title
new_title %in% used_title
new_title
used_title <- c(used_title, new_title)



#### 曝 concordance ####
# as.tokens(list(pu.word.tb$word)) -> pu.tokens
# kwic(pu.tokens, "曝/vc") -> pu.concordance
# write_xlsx(pu.concordance, path = "./語料/term_paper_linguistic_data/output/曝concordance.xlsx")

# pu.word.tb %>%
#     select(word) %>%
#     filter(str_detect(word, pattern = "曝.*")) %>%
#     count(word) %>%
#     arrange(desc(n)) -> pu.counts.tb
# write_xlsx(pu.counts.tb, "semantics_term_paper_topics_data/曝詞數量.xlsx")

# pu.word.tb %>%
#     filter(nzchar(word)) %>%
#     filter(!str_detect(word, "[^\u4E00-\u9FFF_]")) -> pu.word.tb2

# pu.word.tb2 %>%
#     select(word) %>%
#     as.list() %>%
#     as.tokens() %>%
#     tokens_ngrams(n = 2, skip=0, concatenator = "_") %>%
#     as.list() -> pu.bigram

# pu.bigram %>%
#     data.frame() %>%
#     filter(str_detect(word, "曝")) %>%
#     count(word) %>%
#     arrange(desc(n)) -> pu.bigram.counts
# write_xlsx(pu.bigram.counts, "semantics_term_paper_topics_data/曝bigram數量.xlsx")
# 
# pu.bigram.counts %>%
#     filter(str_detect(word, "曝_.+"))

# pu.counts.tb %>%
#     filter(n >= 30) %>%
#     wordcloud2()

#### 炸 ####
ja.guo <- read_xlsx("./semantics_term_paper_topics_data/ws_output/ptt_炸鍋.xlsx")

ja.guo %>%
    unnest_tokens(
        input = para_ckip,
        output = word,
        token = function(x)
            str_split(x, pattern = " ")
    ) -> ja.guo.word.tb

as.tokens(list(ja.guo.word.tb$word)) -> ja.guo.tokens
kwic(ja.guo.tokens, "炸鍋") -> ja.guo.concordance
write_xlsx(ja.guo.concordance, path = "semantics_term_paper_topics_data/炸鍋concordance.xlsx")


## self defined ngram tokenizer
tokenizer_ngrams <-
    function(texts,
             jiebar,
             n = 2 ,
             skip = 0,
             delimiter = "_") {
        texts %>% ## chunks-based char vector
            segment(jiebar) %>% ## word tokenization 
            as.tokens %>% ## list to tokens
            tokens_ngrams(n, skip, concatenator = delimiter) %>%  ## ngram tokenization
            as.list ## tokens to list
    }

# 測試例子
library("jiebaR")
text <- c("這是一個測試的句子",
           "這句子",
           "超短句",
           "最後一個超長的句子測試")

seg1 <- worker(bylines = T)
seg2 <- worker()
result <- segment(text, jiebar = seg1)
result2

result %>%
    as.tokens %>%
    tokens_ngrams(2, skip = 0, concatenator = "_") %>%
    as.list() 

    


#### BP analysis ####
# library(Rling); library(cluster); library(pvclust); library(vcd)

bao.final <- read_excel("./語料/term_paper_linguistic_data/output/最終分析資料_爆2nd.xlsx")
pu.final <- read_excel("./語料/term_paper_linguistic_data/output/最終分析資料_曝2nd.xlsx")

bao.final %>% 
    mutate(target_word = ifelse(VOICE == "active", "bao_active", "bao_passive"), .before = title_id) %>%
    select(-VOICE) -> bao.final

pu.final %>%
    mutate(target_word = ifelse(VOICE == "active", "pu_active", "pu_passive"), .before = title_id) %>%
    select(-VOICE) -> pu.final

bao_col_names <- names(bao.final)
pu_col_names <- names(pu.final)

bao.final[bao_col_names] <- lapply(bao.final[bao_col_names], factor)
pu.final[pu_col_names] <- lapply(pu.final[pu_col_names], factor)

## 把爆&曝tb合併
bao.pu.final <- rbind(bao.final, pu.final)

pu_active <- bao.pu.final[bao.pu.final$target_word == "pu_active", -1:-3]
pu_active.bp <- bp(pu_active)

pu_passive <- bao.pu.final[bao.pu.final$target_word == "pu_passive", -1:-3]
pu_passive.bp <- bp(pu_passive)

bao_active <- bao.pu.final[bao.pu.final$target_word == "bao_active", -1:-3]
bao_active.bp <- bp(bao_active)

bao_passive <- bao.pu.final[bao.pu.final$target_word == "bao_passive", -1:-3]
bao_passive.bp <- bp(bao_passive)

bps.mt <- rbind(pu_active.bp, pu_passive.bp, bao_active.bp, bao_passive.bp)
rownames(bps.mt) <- levels(bao.pu.final$target_word)
bps.mt

bps.dist <- dist(bps.mt, method = "canberra")

bps.hc <- hclust(bps.dist, method = "ward.D2")
bps.hc

plot(bps.hc, hang = -1, xlab = "BP vector difference")

test.clust <- cutree(tree = bps.hc, k = 2)
test.clust

summary(silhouette(test.clust, bps.dist))$avg.width

asw <- sapply(2:3, function(x) summary(silhouette(cutree(bps.hc, k = x), bps.dist))$avg.width)
asw

rect.hclust(bps.hc, k = 2) # 分群最好是2

## bao, pu : active vs. bao, pu: passive
b1 <- bps.mt[c(1,3),] # bao_pu_active
b2 <- bps.mt[-c(1,3),] # bao_pu_passive

b1.bp <- colMeans(b1)
b2.bp <- colMeans(b2)
diff3 <- b1.bp - b2.bp
sort(diff3, decreasing = T)

plot(sort(diff3)*1.2, 1:length(diff3), type = "n", xlab = "cluster pu_voice <–-> cluster bao_voice", yaxt = "n", ylab = "")
text(sort(diff3), 1:length(diff3), names(sort(diff3)))
abline(v=c(0.2, -0.2), col="red")

data.frame(
    "bao_pu_voice_diff" = as.numeric(sort(diff3)),
    "features" = as.character(names(sort(diff3)))
) %>%
    mutate(features = factor(features, levels = features)) -> diff.df3

diff.df3 %>%
    ggplot(aes(bao_pu_voice_diff, features)) +
    geom_point() +
    geom_label_repel(label=diff.df3$features,
               nudge_x = 0.15, nudge_y = 0.5,
               min.segment.length = 0, max.overlaps = Inf,
               size = 3) +
    geom_vline(xintercept = c(-0.2, 0.2), color = "red", linetype = "longdash") +
    theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    xlab("cluster bao_pu_passive <–-> cluster bao_pu_active")# 此圖可用

## bao/pu active vs. bao/pu passive : effect size
cluster2 <- as.character(bao.pu.final$target_word)
cluster2[cluster2 == "bao_active" | cluster2 == "pu_active"] = "1"
cluster2[cluster2 != "1"] = "2"
cluster2 <- as.factor(cluster2)
assocstats(table(cluster2, bao.pu.final$SUBJECT))[5] # 0.528
assocstats(table(cluster2, bao.pu.final$COMPLEMENT))[5] # 0.647
assocstats(table(cluster2, bao.pu.final$SENT_TYPE)) # 0.38
assocstats(table(cluster2, bao.pu.final$TRANSITIVITY))[5] # 0.481
assocstats(table(cluster2, bao.pu.final$PREDADV)) # 0.033
assocstats(table(cluster2, bao.pu.final$ARG_SENT)) # 0.241
assocstats(table(cluster2, bao.pu.final$SUBJ_CAT)) # 0.015

assocstats(table(cluster2, bao.pu.final$SUBJECT))[2]$chisq_tests[1,3]


chisq.test(table(cluster2, bao.pu.final$COMPLEMENT))
chisq.test(table(cluster2, bao.pu.final$SUBJECT))
chisq.test(table(cluster2, bao.pu.final$TRANSITIVITY))

## bao vs. pu
bao.final2 <- read_excel("./語料/term_paper_linguistic_data/output/最終分析資料_爆2nd.xlsx")
pu.final2 <- read_excel("./語料/term_paper_linguistic_data/output/最終分析資料_曝2nd.xlsx")

bao.final2 %>% 
    mutate(target_word = "bao", .before = title_id) -> bao.final2

pu.final2 %>%
    mutate(target_word = "pu", .before = title_id) -> pu.final2

bao_col_names <- names(bao.final2)
pu_col_names <- names(pu.final2)

bao.final2[bao_col_names] <- lapply(bao.final2[bao_col_names], factor)
pu.final2[pu_col_names] <- lapply(pu.final2[pu_col_names], factor)

bao.pu.final2 <- rbind(bao.final2, pu.final2)

pu <- bao.pu.final2[bao.pu.final2$target_word == "pu", -1:-3]
pu.bp <- bp(pu)

bao <- bao.pu.final2[bao.pu.final2$target_word == "bao", -1:-3]
bao.bp <- bp(bao)

bao.pu.bps <- rbind(bao.bp, pu.bp)
rownames(bao.pu.bps) <- levels(bao.pu.final2$target_word)

bao.pu.bps %>%
    rbind(diff = bao.pu.bps[1,] - bao.pu.bps[2,]) -> bao.pu.bps2


sort(bao.pu.bps2[3,])


bao.pu.bps %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("features") %>%
    mutate(differences = bao - pu) %>%
    select(features, differences) %>%
    arrange(differences) %>%
    mutate(features = factor(features, levels = features)) -> diff.df

as.numeric(sort(bao.pu.bps2[3,]))


## ggplot做snake plot => differences
# library(ggrepel)
diff.df %>%
    ggplot(aes(x = differences, y = features)) +
    geom_point() +
    geom_label_repel(label=diff.df$features,
               nudge_x = -0.01, nudge_y = 0.5,
               min.segment.length = 0, max.overlaps = Inf,
               size = 3) +
    geom_vline(xintercept = c(-0.1, 0.1), color = "red", linetype = "longdash") +
    theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    xlab("cluster pu <–-> cluster bao")


## bao vs pu : effect size
cluster1 <- as.character(bao.pu.final2$target_word)
cluster1[cluster1 == "bao"] = "1"
cluster1[cluster1 != "1"] = "2"
cluster1 <- as.factor(cluster1)
assocstats(table(cluster1, bao.pu.final2$COMPLEMENT))
assocstats(table(cluster1, bao.pu.final2$VOICE))
assocstats(table(cluster1, bao.pu.final2$SUBJECT))

assocstats(table(cluster1, bao.pu.final2$VOICE))[[5]]
assocstats(table(cluster1, bao.pu.final2$TRANSITIVITY))[[5]]
assocstats(table(cluster1, bao.pu.final2$SENT_TYPE))[[5]]
assocstats(table(cluster1, bao.pu.final2$PREDADV))[[5]]
assocstats(table(cluster1, bao.pu.final2$SUBJECT))[[5]]
assocstats(table(cluster1, bao.pu.final2$SUBJECT))[[5]]
assocstats(table(cluster1, bao.pu.final2$COMPLEMENT))[[5]]
assocstats(table(cluster1, bao.pu.final2$ARG_SENT))[[5]]
assocstats(table(cluster1, bao.pu.final2$SUBJ_CAT))[[5]]

bao_col_names[4:11]

data.frame(
    "ID_tags" = bao_col_names[4:11],
    "Cramers_V" = c(assocstats(table(cluster1, bao.pu.final2$VOICE))[[5]],
                     assocstats(table(cluster1, bao.pu.final2$TRANSITIVITY))[[5]],
                     assocstats(table(cluster1, bao.pu.final2$SENT_TYPE))[[5]],
                     assocstats(table(cluster1, bao.pu.final2$PREDADV))[[5]],
                     assocstats(table(cluster1, bao.pu.final2$SUBJECT))[[5]],
                     assocstats(table(cluster1, bao.pu.final2$COMPLEMENT))[[5]],
                     assocstats(table(cluster1, bao.pu.final2$ARG_SENT))[[5]],
                     assocstats(table(cluster1, bao.pu.final2$SUBJ_CAT))[[5]])
) %>%
    arrange(desc(Cramers_V))


# (c("voice", "transitivity", "sent_type", "predadv", "subject","complement", "arg_sent", "subj_cat"))
assocplot(table(cluster1, bao.pu.final2$COMPLEMENT))
chisq.test(table(cluster1, bao.pu.final2$COMPLEMENT)) -> bao_pu_complement_chisq
bao_pu_complement_chisq$p.value
chisq.test(table(cluster1, bao.pu.final2$COMPLEMENT))


bao.pu.final2 %>%
    count(COMPLEMENT) %>%
    arrange(desc(n))



