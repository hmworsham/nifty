library(dplyr)

s <- read.csv('~/Downloads/XXXXXXXXX716_Checking_Transactions_20230107-180123.csv', 
              sep=',',
              skip=5,
              row.names=NULL)

colnames(s) <- c('date', 
                 'type', 
                 'check_number', 
                 'description', 
                 'withdrawal',
                 'deposit',
                 'balance')

s$date <- as.Date(s$date, format="%m/%d/%Y")
View(s)


# Build simple regexp strings
# Do this for as many useful classes as you can think of
wages <- 'GUILD .* DIRECT .*|UNIVERSITY OF CA .*'
interest <- 'INTEREST'
credit <- 'BARCLAY'
childcare <- 'HOMEPAY'
xcel <- 'XCEL'
water <- 'WATER'

# Add a class field to the data, default "other"
s$class <- "Other"

# Apply the regexp and return their class
s$class <- ifelse(grepl(wages, s$desc), 'Wages',
                  ifelse(grepl(credit, s$desc), 'CreditCard',
                         ifelse(grepl(childcare, s$desc), 'Childcare', 
                                ifelse(grepl(xcel, s$desc), 'Energy',
                                       ifelse(grepl(water, s$desc), 'Water',
                                "Other")))))

unique(s$description)
