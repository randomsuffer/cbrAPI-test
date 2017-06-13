# hello
rm(list = ls()) #очистить workspace
cat("\014") #очистить консоль

install.packages('plyr')
install.packages('foreign')
install.packages('XML')
install.packages('devtools')
install.packages('ggplot2')

library('plyr')
library('foreign')
library('XML')
library('devtools')
library('ggplot2')

install_github('randomsuffer/cbrAPI')
library('cbrAPI')

# https://github.com/randomsuffer/cbrAPI

# Take some date
# http://cbr.ru/credit/forms.asp
data <- download.dbf("2017-01")

# Leave only the desired columns
data <- data[ , which(names(data) %in% c('REGN','NUM_SC','IITG','A_P','DT'))]
head(data)
tail(data)

# Multiply all the assets of (-1) to find the sum. ITGAP rows (total for asset / liability) will become NA values — it's not terrible.
data$NUM_SC <- as.integer(as.character(data$NUM_SC))
data$IITG <- as.integer(as.character(data$IITG))
x <- subset(data, subset=A_P==1)
y <- subset(data, subset=A_P==2)
x$IITG <- x$IITG * (-1)
data <- rbind(x,y)
data <- data[complete.cases(data),]
data <- data[with(data, order(DT, REGN, NUM_SC)), ]
rm(x,y)

# Take a few required categories of accounts
#  - deposits of physical persons (423, 426 accounts)
#  - deposits of legal entities (420 - 422 and 425 accounts)
#  - equity (passive 100 accounts)
#  - balance profit (701 - 704 and 706 accounts)
# http://www.profbanking.com/info/835-chart-of-accounts-in-banks.html
sub1 <- subset(data, subset=(NUM_SC %in% c(42300:42399, 42600:42699)))
sub2 <- subset(data, subset=(NUM_SC %in% c(42000:42299, 42500:42599)))
sub3 <- subset(data, subset=A_P==2)
sub3 <- subset(data, subset=(NUM_SC %in% c(10200:10999)))
sub4 <- subset(data, subset=(NUM_SC %in% c(70100:70499, 70600:70699)))

data.small <- matrix(nrow=0, ncol=5)
colnames(data.small) <- c('REGN', 'individuals', 'corporate', 'equity', 'profit')
for (i in levels(as.factor(data$REGN))) {
  individuals <- subset(sub1, subset=(REGN==i))
  corporate <- subset(sub2, subset=(REGN==i))
  equity <- subset(sub3, subset=(REGN==i))
  profit <- subset(sub4, subset=(REGN==i))
  data.small <- rbind(data.small, as.integer(c(i, sum(individuals$IITG),
                                               sum(corporate$IITG),
                                               sum(equity$IITG),
                                               sum(profit$IITG))))
}
rm(sub1, sub2, sub3, sub4, individuals, corporate, equity, profit, i)
data.small <- as.data.frame(data.small, stringsAsFactors = FALSE)
head(data.small, 50)

model <- lm(profit ~ individuals + corporate + equity, data = data.small)
options(scipen=10)
coef(model)
summary(model)

predicted.profit <- predict(model, data.small)
actual.profit <- data.small$profit         # сохраняем вектор цен в отдельную переменную для удобства 
plot(predicted.profit, actual.profit, xlim = c(0,200000000), ylim = c(-50000000,100000000)) # предсказанная стоимость vs. цены из имеющихся данных
par(new=TRUE, col="red")           # параметры графика: рисовать на той же конве, использовать красный
dependency <- lm(predicted.profit ~ actual.profit)  # ещё одна модель, на этот раз вспомогательная
abline(dependency)                 # отображаем вспомогательную модель в виде линии

sorted <- sort(predicted.profit / actual.profit, decreasing = TRUE)
sorted[1:10]


