# Dataset was taken from here originallay
# and required a big of handballing

# 'http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/cmudict-0.7b', fill = T)

library(tidyverse)
getwd()
cmu <- read.csv(file = '/Users/cexsligo/Documents/Data Analytics Nci/Data Analysis Files/Carnegie Wordlist.csv', 
                header = T, 
                stringsAsFactors = F)
glimpse(cmu)

#merge the phonemes into a phonological form
cmu$phonological <- paste(cmu$V2, cmu$V3, cmu$V4, cmu$V5, cmu$V6, cmu$V7, cmu$V8, cmu$V9, cmu$V10, cmu$V11, cmu$V12,
                          cmu$V13, cmu$V14, cmu$V15, cmu$V16, cmu$V17, cmu$V18, cmu$V19, cmu$V20,
                          cmu$V21, cmu$V22, cmu$V23, cmu$V24, cmu$V25, cmu$V26, cmu$V27, cmu$V28, cmu$V29, 
                          cmu$V30, cmu$V31, cmu$V32, cmu$V33)

#removing punctuation and numbers from the column of words 

cmu$V1 <- gsub("[[:punct:]]", "", cmu$V1)
cmu$V1 <- gsub("[0-9]", "", cmu$V1)

#create vector with the number of letters in each word
cmu$V1[35348] <- 'DJ' #have to change 35348 first cos R didn't like the weird scramble that was there "D\xc9J\xc0"

#now we can include the length of each word
cmu$wordLength <- nchar(cmu$V1) 

#This retains only the numerals in the strings
#Hence we can add a column with each word's stress pattern.
cmu$stress <- gsub("[^0-9]", "", cmu$phonological) 
    # This works because Column 2 contains both phonemes and stress patterns

#no syllables in each word 
cmu$syllables <- nchar(as.character(cmu$stress ))

#marked with 0, 1 or 2. 
cmu$stress <- as.factor(cmu$stress) #naturally we want this to be a factor
cmu$stress <- sapply(cmu$stress , function(x) paste0("'", x)) #I'm sticking an apostrophe before each stress pattern
#because Excel banjoes the leading zeroes and then you can't read back in the data 

levels(as.factor(cmu$stress)) # a whopping 285)
#probably worth clustering but I haven't do so yet


#Time to convert words to binary
cmu$CV <- gsub ('[QWRTYPLKJHGFDSZXCVBNM]', 'C' , cmu$word) #replaces all vowels with 'C'
cmu$CV <- gsub ('[AEIOU]', 'V' , cmu$CV) #replaces all vowels with 'V'
cmu$CV  <- as.factor(cmu$CV )

ncol(cmu) #39
glimpse(cmu)
#renaming the columns now
names(cmu) <- c('row', 'word',
                'p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8','p9',
                'p10', 'p11', 'p12', 'p13', 'p14', 'p15', 'p16', 'p17', 'p18', 'p19', 'p20',
                'p21', 'p22', 'p23', 'p24', 'p25', 'p26', 'p27', 'p28', 'p29', 'p30', 'p31', 'p32',
                'phonForm','wordLength','stressPattern',
                'noSyls', 'CV'  )

cmu <- cmu[,c(1,2, 39, 36:38, 35, 3:34)] #reordering the data 
#so that stress and phonological columns are to the left of the dfrm


cmu[3456,] #view random (Amen, as it happens)
cmu[213:245,] #having a look at some middle of the dataset stuff. 
# 'Abkhazia' and the likes
# notice how the same words get repeated and there are lots of plurals etc. 

write.csv(cmu, file = 'CarnegieData with Stress Patterns.csv')

# Next we need to hyphenate the flock out of our words
# _____________________________________________________


      #here's one I prepared earlier: 
      # twoSyl <- read_csv(file = 'Bisyllables of English.csv') #this was for my own storage.
      # glimpse(twoSyl)

library(stringr)
install.packages('sylly.en')
library(sylly.en) #this is a rare package used for hyphenating English words
# https://github.com/unDocUMeantIt/sylly/blob/master/R/sylly-internal.R

                    # #here is a sample for the uninitiated
                    # sampleText <- c("This", "is", "a", "rather", "stupid", "demonstration")
                    # sampleHyph<- hyphen_df(sampleText, hyph.pattern="en")
                    # 
                    # # > sampleHyph
                    # syll             word
                    # 1    1             This
                    # 2    1               is
                    # 3    1                a
                    # 4    2          rath-er
                    # 5    2          stu-pid
                    # 6    4 de-mon-stra-tion
                    # # 


#the next problem is now how to map from this to the syllable structure. 

#subset of cmu - all 2syllable words

twoSyl <- select(cmu, 1:7) %>% 
  filter(cmu$noSyls == '2') #just to store for later

#run the vector through the hyphenator
twoSyl_hyph <- hyphen_df(twoSyl$word, hyph.pattern="en") # this is mighty (slow)
# returns a dfrm 
# which needs to be added to the base dfrm
names(twoSyl_hyph) <- c('hyphSylls', 'hyphed' )
twoSyl <- cbind(twoSyl, twoSyl_hyph)

#Time to convert words to CV form
twoSyl$hyphCV <- gsub ('[QWRTYPLKJHGFDSZXCVBNM]', 'c' , twoSyl$hyphed) 
twoSyl$hyphCV <- gsub ('[AEIOU]', 'v' , twoSyl$hyphCV) 

# A bunch of these words don't have two hyphens (due to )
write.csv(twoSyl, file = 'Bisyllables of English_all_hyphense.csv') #this was for my own storage.

#otherwise I would have to run it through the mighty (slow) hyphenator


twoSyl <- filter(twoSyl, hyphSylls == 2) 
#this gets rid of mismatches between the original and the column that went through the hyphenator
# we need this for below (not the version saved to CSV)

#split the CVed hyphenated form into two parts 
letsPerSyl <- as.data.frame(str_split_fixed(twoSyl$hyphCV, '-', 2))
twoSyl <- cbind(twoSyl, letsPerSyl )
ncol(twoSyl)
names(twoSyl) [c(11, 12)]<- c('CVs1', 'CVs2')
twoSyl$noCVs1 <- nchar(as.character(twoSyl$CVs1))
twoSyl$noCVs2 <- nchar(as.character(twoSyl$CVs2))
glimpse(twoSyl)
write.csv(twoSyl, file = 'Bisyllables of English_two_hyphens_only.csv') #this was for my own storage.



===========================================
  Time for Data Viz
==========================================

  
  twoSyl 


x <- as.matrix(table(twoSyl$hyphCV, twoSyl$stressPattern ))
# don't plot(x). It breaks your computer

barplottable(polySyl$stressPattern)
t <- as.data.frame(table(polySyl$stressPattern))
t < filter (t, t$Freq > 5)
t <- sort(t$Freq, decreasing = T)
barplot (t$Freq)
attach(polySyl)
x <- as.data.frame(table(noCVs1, noCVs2, stressPattern))
x <- filter(x, x$Freq > 0) 

# # time to do a bubble plot for each stress pattern. 
# -----------------------------------------------------   
# #using various colours
# ggplot(x, aes(x=noCVs2, y=noCVs1, size = prop, color = stressPattern)) +
#   geom_point(alpha = 0.3, color = x$stressPattern)+  
#   scale_size_continuous(range = c(1, 7)) 
# the legend needs to include the stress pattern by colour
# but either way it's a pretty confusing plot, especially with four groups


#remove disqualified stress patterns which come with the cmu dataset
# 
levels(x$stressPattern)
x <- filter(x, stressPattern != '00' & 
              stressPattern != '22' &
              stressPattern != '20' &
              stressPattern != '11'  &
              stressPattern != '02') 

x <- filter(x, stressPattern != '10' )

#calculate proportions of each variable pairing across the Stress Pattern factor
library(plyr)     
x<-ddply(x,.(noCVs1,noCVs2),transform,prop=Freq/sum(Freq))

# solution from https://stackoverflow.com/questions/15009011/calculate-proportions-within-subsets-of-a-data-frame


#Manually reorder factor levels.
# this is so that we get two pairs in the graphs
# becus 10 and 12 are kindof a pair and so are 01 and 21
x$stressPattern <- fct_relevel(x$stressPattern, c("01","21", "00" , "02", "10","12", "11", "20",  "22"))
library(forcats)


#and now for a lattice graph of bar charts
# ----------------------------------------
# Numeric variable (Freq) on each y axis()
# categoric on x (stressPattern)
# grid of 2 factor variable (no letters in each of the two sylls)

ggplot(data= x, aes(x=stressPattern, y=prop, fill = stressPattern)) + #default ggplot colours work well 
  geom_bar(stat="identity", 
           color = 'black') +
  facet_grid( noCVs2 ~ noCVs1)  + #(no letters in each of the two sylls)
  coord_flip()+
  theme_bw()


# this time I want to remove initial consonants
# ---------------------------------------------
#and then we'll do the facet_grid with 
# hotel_manager
# hotel-manager
# 
# #just having some thoughts on best practices
# #i might be coming around to the underscore
# +

twoSyl$hyph_rhyme1 <- gsub ('^[c]*', '' , twoSyl$CVs1)  
twoSyl$hyph_rhyme2 <- gsub ('^[c]*', '' , twoSyl$CVs2)  

twoSyl$noVCs1 <- nchar(as.character(twoSyl$hyph_rhyme1))
twoSyl$noVCs2 <- nchar(as.character(twoSyl$hyph_rhyme2))

# it took me ages to get this right but it turns out that I was 
# https://stackoverflow.com/questions/9704213/remove-part-of-a-string
glimpse(twoSyl)
# twoSyl$hyph_rhyme <- str_replace("^[c]*", "", twoSyl$CVs1) Tidyverse equivalent

#now we repeat the same as earlier, only with the newly edited columns
attach(twoSyl)
x <- as.data.frame(table(noVCs1, noVCs2, stressPattern))
x <- filter(x, x$Freq > 0) 

#remove disqualified stress patterns which come with the cmu dataset
# 
levels(x$stressPattern)
x <- filter(x, stressPattern != '00' & 
              stressPattern != '22' &
              stressPattern != '20' &
              stressPattern != '11'  &
              stressPattern != '02') 

wou$word <- 'breaking'
# x <- filter(x, stressPattern != '10' ) #might do this later

#calculate proportions of each variable pairing across the Stress Pattern factor
library(plyr) 
library(forcats)

x<-ddply(x,.(noVCs1,noVCs2),transform,prop=Freq/sum(Freq))

# solution from https://stackoverflow.com/questions/15009011/calculate-proportions-within-subsets-of-a-data-frame


#Manually reorder factor levels.
# this is so that we get two pairs in the graphs
# becus 10 and 12 are kindof a pair and so are 01 and 21
# x$stressPattern <- fct_relevel(x$stressPattern, c("01","21", "00" , "02", "10","12", "11", "20",  "22"))


#and now for a lattice graph of bar charts
# ----------------------------------------
# Numeric variable (Freq) on each y axis()
# categoric on x (stressPattern)
# grid of 2 factor variable (no letters in each of the two sylls)

ggplot(data= x, aes(x=stressPattern, y=prop, fill = stressPattern)) + #default ggplot colours work well 
  geom_bar(stat="identity", 
           color = 'black') +
  facet_grid( noVCs2 ~ noVCs1)  + #(no letters in each of the two sylls)
  coord_flip()+
  theme_bw()

# Let's do the same for absolute values, not proportions
ggplot(data= x, aes(x=stressPattern, y=Freq, fill = stressPattern)) + #default ggplot colours work well 
  geom_bar(stat="identity", 
           color = 'black') +
  scale_y_continuous(trans='log1.1') +
  facet_grid( noVCs2 ~ noVCs1)  + #(no letters in each of the two sylls)
  coord_flip()+
  theme_bw()

glimpse (twoSyl)
head(as.data.frame(twoSyl$phonForm, twoSyl$word))
f[2345:2367,]

f <- filter(twoSyl, twoSyl$word == 'ARTHUR'|
              twoSyl$word == 'BEGETS'|
              twoSyl$word == 'BANTER') %>% 
  select( c(9, 3, 5, 12, 13, 14, 15, 6))
glimpse(f)




# Repeat the same for more than three syllables
# ==============================================
# Here's the output from below
# I did this to save myself the misery of using the hyphenator again
# polySyl <- read_csv(polySyl, file = 'trisyllables of English.csv', col_names = T) 
# glimpse(polySyl)

polySyl <- select(cmu, 1:7) %>% 
  filter(cmu$noSyls > '2') #this is developed later 

#=hyphenate
polySyl_hyph <- hyphen_df(polySyl$word, hyph.pattern="en") # this is mighty slow
 # and you want to keep it in storage 

# returns a dfrm 
# which needs to be added to the base dfrm
names(polySyl_hyph) <- c('hyphSylls', 'hyphed' )
polySyl <- cbind(polySyl, polySyl_hyph)

#Time to convert words to CV form
polySyl$hyphCV <- gsub ('[QWRTYPLKJHGFDSZXCVBNM]', 'c' , polySyl$hyphed) 
polySyl$hyphCV <- gsub ('[AEIOU]', 'v' , polySyl$hyphCV) 


# A bunch of these words don't have two hyphens (due to )

write.csv(polySyl, file = 'trisyllables of English-all hyphens.csv') #this was for my own storage.
#i'm now off to do my decision tree using the last nine columns 
nrow(polySyl) #55396
polySyl <- filter(polySyl, hyphSylls > 2)  #45889 now
#this gets rid of mismatches between the original and the column that went through the hyphenator
# we need this for below (not the version saved to CSV)


#split the CVed hyphenated form into 9 parts,
#now each syllable will be represented by CV VCC CV etc. 
letsPerSyl3 <- as.data.frame(str_split_fixed(polySyl$hyphCV, '-', 9))

polySyl <- cbind(polySyl, letsPerSyl3) #add them together

#this function counts the number of characters in each cell of a vector
noLetters <- function (x){
  y <- nchar(as.character(x))
}
letsPerSyl3 <- sapply(letsPerSyl3, noLetters)  # A cheeky hack cos i'm too tird to look up the aggregatge function 
polySyl <- cbind(polySyl, letsPerSyl3) #sic
ncol(polySyl) #18

write.csv(polySyl, file = 'trisyllables of English_two_hyphens_only.csv') #this was for my own storage.
#i'm now off to do my decision tree using the last nine columns 


===========================================
  Time for Data Viz
==========================================
  
  barplottable(polySyl$stressPattern)
t <- as.data.frame(table(polySyl$stressPattern))
t < filter (t, t$Freq > 5)
t <- sort(t$Freq, decreasing = T)
barplot (t$Freq)
attach(polySyl)
x <- as.data.frame(table(noCVs1, noCVs2, stressPattern))
x <- filter(x, x$Freq > 0) 

# # time to do a bubble plot for each stress pattern. 
# -----------------------------------------------------   
# #using various colours
# ggplot(x, aes(x=noCVs2, y=noCVs1, size = prop, color = stressPattern)) +
#   geom_point(alpha = 0.3, color = x$stressPattern)+  
#   scale_size_continuous(range = c(1, 7)) 
# the legend needs to include the stress pattern by colour
# but either way it's a pretty confusing plot, especially with four groups


#remove disqualified stress patterns which come with the cmu dataset
# 
levels(x$stressPattern)
x <- filter(x, stressPattern != '00' & 
              stressPattern != '22' &
              stressPattern != '20' &
              stressPattern != '11'  &
              stressPattern != '02') 

x <- filter(x, stressPattern != '10' )

#calculate proportions of each variable pairing across the Stress Pattern factor
library(plyr)     
x<-ddply(x,.(noCVs1,noCVs2),transform,prop=Freq/sum(Freq))

# solution from https://stackoverflow.com/questions/15009011/calculate-proportions-within-subsets-of-a-data-frame


#Manually reorder factor levels.
# this is so that we get two pairs in the graphs
# becus 10 and 12 are kindof a pair and so are 01 and 21
x$stressPattern <- fct_relevel(x$stressPattern, c("01","21", "00" , "02", "10","12", "11", "20",  "22"))
library(forcats)


#and now for a lattice graph of bar charts
# ----------------------------------------
# Numeric variable (Freq) on each y axis()
# categoric on x (stressPattern)
# grid of 2 factor variable (no letters in each of the two sylls)

ggplot(data= x, aes(x=stressPattern, y=prop, fill = stressPattern)) + #default ggplot colours work well 
  geom_bar(stat="identity", 
           color = 'black') +
  facet_grid( noCVs2 ~ noCVs1)  + #(no letters in each of the two sylls)
  coord_flip()+
  theme_bw()


# this time I want to remove initial consonants
# ---------------------------------------------
#and then we'll do the facet_grid with 
# hotel_manager
# hotel-manager
# 
# #just having some thoughts on best practices
# #i might be coming around to the underscore
# +

twoSyl$hyph_rhyme1 <- gsub ('^[c]*', '' , twoSyl$CVs1)  
twoSyl$hyph_rhyme2 <- gsub ('^[c]*', '' , twoSyl$CVs2)  

twoSyl$noVCs1 <- nchar(as.character(twoSyl$hyph_rhyme1))
twoSyl$noVCs2 <- nchar(as.character(twoSyl$hyph_rhyme2))

# it took me ages to get this right but it turns out that I was 
# https://stackoverflow.com/questions/9704213/remove-part-of-a-string
glimpse(twoSyl)
# twoSyl$hyph_rhyme <- str_replace("^[c]*", "", twoSyl$CVs1) Tidyverse equivalent

#now we repeat the same as earlier, only with the newly edited columns
attach(twoSyl)
x <- as.data.frame(table(noVCs1, noVCs2, stressPattern))
x <- filter(x, x$Freq > 0) 

#remove disqualified stress patterns which come with the cmu dataset
# 
levels(x$stressPattern)
x <- filter(x, stressPattern != '00' & 
              stressPattern != '22' &
              stressPattern != '20' &
              stressPattern != '11'  &
              stressPattern != '02') 

wou$word <- 'breaking'
# x <- filter(x, stressPattern != '10' ) #might do this later

#calculate proportions of each variable pairing across the Stress Pattern factor
library(plyr) 
library(forcats)

x<-ddply(x,.(noVCs1,noVCs2),transform,prop=Freq/sum(Freq))

# solution from https://stackoverflow.com/questions/15009011/calculate-proportions-within-subsets-of-a-data-frame


#Manually reorder factor levels.
# this is so that we get two pairs in the graphs
# becus 10 and 12 are kindof a pair and so are 01 and 21
# x$stressPattern <- fct_relevel(x$stressPattern, c("01","21", "00" , "02", "10","12", "11", "20",  "22"))


#and now for a lattice graph of bar charts
# ----------------------------------------
# Numeric variable (Freq) on each y axis()
# categoric on x (stressPattern)
# grid of 2 factor variable (no letters in each of the two sylls)

ggplot(data= x, aes(x=stressPattern, y=prop, fill = stressPattern)) + #default ggplot colours work well 
  geom_bar(stat="identity", 
           color = 'black') +
  facet_grid( noVCs2 ~ noVCs1)  + #(no letters in each of the two sylls)
  coord_flip()+
  theme_bw()

# Let's do the same for absolute values, not proportions
ggplot(data= x, aes(x=stressPattern, y=Freq, fill = stressPattern)) + #default ggplot colours work well 
  geom_bar(stat="identity", 
           color = 'black') +
  scale_y_continuous(trans='log1.1') +
  facet_grid( noVCs2 ~ noVCs1)  + #(no letters in each of the two sylls)
  coord_flip()+
  theme_bw()

glimpse (twoSyl)
head(as.data.frame(twoSyl$phonForm, twoSyl$word))
f[2345:2367,]

f <- filter(twoSyl, twoSyl$word == 'ARTHUR'|
              twoSyl$word == 'BEGETS'|
              twoSyl$word == 'BANTER') %>% 
  select( c(9, 3, 5, 12, 13, 14, 15, 6))
glimpse(f)



