# 4305datathon
How to use this github. 

1. Copy this github into your own workspace folder. You can use ```git clone <sshlink>``` in your terminal or download the ZIP. Most importantly the file structure needs to be exactly the same for the commands at the bottom to work!

Not everything is in one file so you need to pay attention.

1. cleaning.R contains a function to convert the 2023-26 raw data into the cleaned data.
2. util.R constains a function that sets the correct types for common variables for all the pricing books.
    - Note 2028 book has a NewBusinessFlag which you manually set to boolean
3. The full dataset for each year will be named bookYY where YY represents the year. The common importing workflow would look like.

```{r}
source("util.R")

book26 = read.csv("data/book26.csv", header = T)
book26 = set_types(book26)
```
```{r}
# for all the data
book26 = read.csv("data/book26.csv", header = T)
book27 = read.csv("data/renewal_book_2027.csv", header = T)
book28 = read.csv("data/renewal_book_2028.csv", header = T) # There are new policies hence why there are 1521 NA observations and 1521 new business flags

book26 = set_type(book26)
book27 = set_type(book27)
book28 = set_type(book28)
book28$NewBusinessFlag = as.logical(book28$NewBusinessFlag)
```

