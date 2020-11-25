---
title: "Week 1: Recap Year 1 Skills"
output:
  html_document: default
  keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(kableExtra)
```


## Dataset

This homework will use a built in dataset called USJudgeRatings. The data contains average ratings of 43 different judges' abilities by lawyers in the US. The first column gives the mean number of contacts each judge has had with the lawyers doing the ratings. The following columns give mean ratings out of 10 by the lawyer of the judge’s abilities in different areas of their job. The final column gives the lawyers judgement about whether the judge is worthy of retention.

Much like this week's lab, we will be recapping wrangling skills and inferential tests we did last year.

## 1. Set-up

Load in the tidyverse, broom and `USJudgeRatings` dataset, saving it as `data`

HINT data <- get(data("NAME OF DATASET"))

```{r}
library('tidyverse')

library('broom')

data <- get(data('USJudgeRatings'))
```


Type `USJudgeRatings` into the help panel to find out about the variables contained in this dataframe.

We want to test the hypothesis that lawyers assign higher ratings to judges whom they have more frequent contact with. 

Which column specifies the number of contacts of lawyer with a judge?
ANSWER: the first column, named 'CONT'.

Is the dataframe in WIDE or LONG format?
ANSWER: it's in wide format because each variable has its own column, instead of having 2 columns with one column holding the different variables and the other their values (long format).

## 2. Data wrangling

First remove the column reflecting 'Worthy of retention' as we are not interested in that variable
```{r}
data <- data %>% select(-'RTEN')
```

Next create a new column `id` so that we can identify the judges anonymously (their names are in the row description but tidyverse sometimes removes this information).

```{r}
# source of help: https://stackoverflow.com/questions/23518605/add-an-index-numeric-id-column-to-large-data-frame

data <- data %>% mutate(id=row_number())

```

Next we want to convert the table to the format with three columns: `id`, `variable` and `score`: save this as object data2

HINT use `gather()`
```{r}
# wide to long format
data2 <- data %>% gather('variable', 'score', CONT:PHYS)
```

## 2. Descriptive Statistics

Now we can summarise the data. Make a table of mean, sd, se and 95% confidence intervals for each variable and save it as `summary`.

HINT: use a grouping variable
```{r}
summary <- data2 %>% group_by(variable) %>% summarise(mean=mean(score), sd=sd(score), se = sd/sqrt(n()), CI_95_lower=mean - 1.96*se, CI_95_upper=mean + 1.96*se)
```


Which variable has the most variance?
ANSWER: DMNR is the variable with the most variance in the dataframe: sd = 1.1437054

Which variable has the highest mean?
ANSWER: INTG is the variable with the highest mean in the dataframe: mean = 8.020930


## 3. Further wrangling

In order to run our analysis we need to do a bit more wrangling as we want to create a total rating variable to compare with the CONT variable.

Create a new dataframe (`data3`) that comprises `id` and `total`. Total is the sum score of all variables except number of contacts with judge.

HINT use filter
GROUP BY judge
```{r}
data3 <- data %>% group_by(id) %>% mutate(total=sum(INTG, DMNR, DILG, CFMG, DECI, PREP, FAMI, ORAL, WRIT, PHYS)) %>% select(id, total)
```

Now we will integrate the total with the original dataframe `data`

HINT. inner_join()
```{r}
joined <- inner_join(data, data3, by='id')
```

Now we are ready for our data analysis

## 4. Data visualisation

Make a graph to help you decide whether the total ratings are normally distributed 

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}

ggplot(joined, aes(total)) +
  geom_histogram() +
  theme_bw()

```

* do the total ratings appear normally distributed?
The total ratings (`total` variable) doesn't appear to be normally distributed.

To be sure statistically, we can also run a Shapiro-Wilk Normality Test. If the p-value we obtain is significant (below alpha threshold of .05), then the normality assumption is NOT met.

```{r}
shapiro.test(joined %>% pull(total))
```
As seen above, our p-value <.05, which demonstrates that the normality assumption is not met here.



Next plot the relationship between number of contacts and total rating.

```{r echo=FALSE, warning=FALSE, message=FALSE}
# both of these variables are numeric so we can use a scatter plot to visualise their relationship

ggplot(joined, aes(x=CONT, y=total)) +
  geom_point() +
  theme_bw() +
  labs(title='Relationship between Number of Contacts and Total Rating', x='number of contacts (CONT)', y='total rating (total)')

```

How would you describe the relationship?
ANSWER: It is difficult to say much from the scatter plot we created above, but it seems like there is a slightly positive relationship between the number of contacts and the total rating.


## 5. Inferential statistics: correlation

Run the correlation for CONT and total using `cor.test()` and tidy to a table.

```{r echo=FALSE, message=FALSE, warning=FALSE}

# here, because not all assumptions for pearson's correlation are met (normality assumption is not met), we will run Spearman's correlation
# (for later personal reference: https://towardsdatascience.com/clearly-explained-pearson-v-s-spearman-correlation-coefficient-ada2f473b8#:~:text=The%20fundamental%20difference%20between%20the,with%20monotonic%20relationships%20as%20well.)

cor_results <- cor.test(joined  %>% pull(CONT), joined %>%  pull(total), method='spearman') %>%
  
  tidy()
```

* what is the rho for this correlation?
ANSWER: From the co_results dataframe, we can see that r=-0.05 

* is the result significant?
ANSWER: The result is not significant as p >.05 (p=.75)

* what is the direction and size of the correlation?
ANSWER: The correlation is very slightly negative, and of size equal to 0.05 approximately (low). 
