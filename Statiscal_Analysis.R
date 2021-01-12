
---
  title: "Statistical Analysis of Cold Calls for Car Insurance"
author: "Sai Vineeth K R, Chaitanya, Vinit "
date: "12/1/2019"
output:
  html_document:
  code_folding: hide
highlight: haddock
theme: readable
toc: yes
word_document:
  toc: yes
pdf_document:
  toc: yes
---
  ## Abstract
  The dataset used for the project is from a Bank in the United States. The bank provides services for Car Insurance too. The bank organises events which involves calling the customer and provide information about the insurance scheme. The bank has data about the customer like age, gender, marital status, education, is the customer a current  insurance holder or not, etc. The bank then collects information about the call history with the client so that it can device a model where the bank can target the customers better and get the most number of responses. The project deals with providing a statistical analysis of the dataset along with the hypothesis testing in order to understand the behaviour of the customer.

## Dataset
The dataset has been taken from the website [Kaggle](https://www.kaggle.com/kondla/carinsurance). The dataset has categorical as well as numerical columns. The description of the dataset for each column is given below:
  
  1. **Id** - Unique ID Number
2. **Age** - Age of the client
3. **Job** - Occupation of the client
4. **Marital** - Marital status of the client
5. **Education** - Education level of the client
6. **Default** - Does the client have credit in default?
  7. **Balance** - Average yearly balance of the client
8. **HHInsurance** - Does the client have household insured?
  9. **CarLoan** - Does the client have a car loan?
  10. **Communication** - Method of communication
11. **LastContactMonth** - Month client was last contacted
12. **LastContactDay** - Day client was last contacted
13. **CallStart** - Time at which the call started (HH:MM:SS)
14. **CallEnd** - Time at which the call ended (HH:MM:SS)
15. **NoOfContacts** - Number of contacts performed during this campaign for this client 
16. **DaysPassed** - Number of days passed the client was last contacted
17. **PrevAttempts** - Number of times the client was contacted before the campaigne
18. **Outcome** - Outcome of te previous campaigne
19. **CarInsurance** - Did the client subscribe for the car insurance?
  
  ## Additional Column
  
  1. **Duration** - The duration the call lasted (in seconds)

## Loading dataset and Data Transformation

Loading the libraries.

```{r, message=FALSE}
library(lubridate)
library(ggplot2)
library(fitdistrplus)
library(dplyr)
library(e1071)
library(gridExtra)
library(PASWR)
library(tidyverse)
```

Reading the dataset

```{r}
dataset <- read.csv('../input/carinsurance//carInsurance_train.csv')

```

First few rows of the dataset are as follows.

```{r}
head(dataset)
```

Adding a column Duration to the dataset

```{r}
dataset$Duration = time_length(hms(dataset$CallEnd)) -
  time_length(hms(dataset$CallStart))
```

## Distribution of the continuous columns

We have identified two columns which will be used futher for hypothesis testing. In order to do a hypothesis testing, we'll first need to understand the distribution of the variable. 

* **Duration**

```{r}
ggplot(dataset) +
        geom_density(aes(x = Duration)) +
        theme_bw() + 
        ggtitle('Density curve for Duration') +
        theme(plot.title = element_text(hjust = 0.5))
```

As we can see from the figure, the data is right skewed. For completing a hypothesis testing, the data needs to be normal. We'll accomplish this by taking the log of the column and then removing any heacy tailed values if any.

```{r}
dataset$Duration = log(dataset$Duration)
```

```{r}
ggplot(dataset) +
  geom_density(aes(x = Duration)) +
  theme_bw() +
  ggtitle('Density curve for Duration') +
  theme(plot.title = element_text(hjust = 0.5))
```

We can see there is tail on the left hand side. We can filter the values less than 3 and get a normal curve for the dataset.

```{r}
dataset = dataset %>%
  filter(Duration > 3)
```

Plotting the values

```{r}
ggplot(dataset) +
  geom_density(aes(x = Duration)) +
  theme_bw() +
  ggtitle('Density curve for Duration') +
  theme(plot.title = element_text(hjust = 0.5))
```

* **Balance**
  
  ```{r}
ggplot(dataset) +
  geom_density(aes(x = Balance)) +
  theme_bw() +
  ggtitle('Density curve for Balance') +
  theme(plot.title = element_text(hjust = 0.5))
```

As we can see the curve is not perfectly normal, we'll take the log of the values in the column to remove the skewed values in the distribution.

```{r, message=FALSE}
dataset$Balance = log(dataset$Balance)
```

```{r}
ggplot(dataset) +
        geom_density(aes(x = Balance)) +
        ggtitle('Density curve for Balance (log scale)') +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
```

As we can see, the plot is left skewed. We'll filter out values less than 3.

```{r}
dataset = dataset %>%
  filter(Balance > 3)
```

```{r}
ggplot(dataset) +
  geom_density(aes(x = Balance)) +
  theme_bw() +
  ggtitle('Density curve for Balance (log scale)') +
  theme(plot.title = element_text(hjust = 0.5))
```

## Goodness-of-fit test 

* **Duration**
  
  ```{r}
descdist(dataset$Duration)
```

The Cullen and Frey Graph clearly shows the dataset is close to a normal distribution. We'll reverify it using the QQ-Plot.

```{r}
plot(fitdist(dataset$Duration, 'norm'))
```

The QQ-Plot follows the straight line showing the columns normal beahviour. Hence we conclude that the Duration column follows a normal distribution.

* **Balance**

```{r}
descdist(dataset$Balance)
```

As seen for the Duration column, Behaviour column also has a distribution close to the normal distribution. We'll revierfy it using the QQ-Plot.

```{r}
plot(fitdist(dataset$Balance, 'norm'))
```

The QQ-Plot for the Balance column follows the straight line for the QQ-Plot. Hence we conclude that the distribution of the Balance column is normal distribution.

## Univariate Analysis

The following section tries to derive the insights for the differences between the characterisitics of the client.

```{r}
dataset$LastContactMonth = factor(dataset$LastContactMonth, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))
```

```{r uni1,fig.align="center",fig.width=14,fig.height=14, fig.cap=''}
g1 = ggplot(dataset,aes(x=Age))+
  geom_bar(stat ="count",aes(fill=factor(CarInsurance)))+
  xlab("Age of client")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw()+ggtitle("Distribution of age")

g2 = ggplot(dataset[!is.na(dataset$Job), ],aes(x=Job))+
  geom_bar(stat ="count",aes(fill=factor(CarInsurance)))+
  xlab("Job of client")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw()+ggtitle("Client job")+theme(axis.text.x=element_text(angle=60, hjust=1))

g3 = ggplot(dataset[!is.na(dataset$Education), ],aes(x=Education))+
  geom_bar(stat ="count",aes(fill=factor(CarInsurance)))+
  xlab("Education of client")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw()+ggtitle("Education level")

g4 = ggplot(dataset,aes(x=sign(Balance)*abs(Balance)^(1/3),color=factor(CarInsurance)))+
  geom_density()+
  xlab("Average balance with cube root transformation")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw()+ggtitle("Average balance distribution")

g5 = ggplot(dataset,aes(x=factor(Default)))+
  geom_bar(stat ="count",aes(fill=factor(CarInsurance)))+
  xlab("Credit Default")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw()+ggtitle("Credit default")

g6 = ggplot(dataset,aes(x=factor(HHInsurance)))+
  geom_bar(stat ="count",aes(fill=factor(CarInsurance)))+
  xlab("Household Insurance")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw()+ggtitle("Is Household insured")

g7 = ggplot(dataset,aes(x=factor(CarLoan)))+
  geom_bar(stat ="count",aes(fill=factor(CarInsurance)))+
  xlab("Car Loan")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw() + ggtitle("Does client have car loan")

g8 = ggplot(dataset,aes(x=LastContactMonth))+
  geom_bar(stat ="count",aes(fill=factor(CarInsurance)))+
  xlab("Month")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw() +ggtitle("Last month of contact")

g10 = ggplot(dataset,aes(x=Duration))+
  geom_bar(stat = "identity",binwidth = 5,aes(fill=factor(CarInsurance)))+
  xlab("Duration")+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw() +ggtitle("Call Duration")

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g10,ncol=2)

```

1. **Age** : Most population is between the age of 25 to 60. The distribution of whether car insurance was brought or not seems fairly even  

2. **Job** : Students, unemployed and retired individuals tend to buy car insurance more often than not  

3. **Education** : No specific pattern for opting for car insurance

4. **Average Balance** : With a cube root transformation to normalize the variable. the class distribution looks pretty similar 

5. **Credit Default** : The distribution looks pretty similar for both classes  

6. **Insurance** : Uninsured households are more likely to buy car insurance  

7. **Car Loan** : No specific pattern for opting for car insurance

8. **Month of Contact** : Individuals are more likely to buy car insurance during months  

9. **Day of Contact** : Individuals are more likely to buy insurance at the start of the month  

10. **Call Duration** Longer calls lead to more purchases  


```{r, message=FALSE, warning=FALSE}
ggplot(dataset, aes(x=factor(CarInsurance), y=Duration, fill= factor(CarInsurance))) +
  ylab("Call Duration (log scale)") +
  geom_boxplot()+scale_fill_manual(values=c("#fe5970", "#c2fe59"))+ 
  xlab("Car Insurance") +
  theme(legend.position='none',
        panel.background = element_rect(
          size = 0.5, linetype = "solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Box plot for Duration')
```

```{r, message=FALSE, warning=FALSE}
ggplot(dataset, aes(x=factor(CarInsurance), y=Balance, fill= factor(CarInsurance)))+
  geom_boxplot()+
  scale_fill_manual(values=c("#fe5970", "#c2fe59"))+
  xlab("Car Insurance")+
  ylab("Balance (log scale)")+
  theme(legend.position='none',
        panel.background = element_rect(
          size = 0.9, linetype = "solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Boxplot for Balance')
```

We can clearly see the distinction for the duration the client speaks on the call. The client seems to engage in the call for a longer duration as compared to the duration he doesn't get insured. There isn't any great visible difference between the balance when the client opts for the insurance versus when the client declined to get insured with the bank.

```{r}
dataset$DaysPassed = ifelse(dataset$DaysPassed == -1, 0, dataset$DaysPassed)
```

```{r uni11,fig.align="center",fig.width=12,fig.height=8, warning=FALSE}
ggplot(dataset,aes(x=NoOfContacts,y=PrevAttempts,color=factor(CarInsurance)))+
  geom_point()+
  geom_jitter()+
  geom_smooth()+
  facet_wrap(cut(DaysPassed,breaks = 8)~CarInsurance,ncol = 7)+
  guides(fill=guide_legend(title="Car Insurance"))+
  theme_bw() +
  labs(col = 'Car Insurance')
```

It can be inferred from the above plots, higher previous contacts and lesser duration between campaigns lead to higher purchases and calls after longer durations lead to higher purchases, though we cannot substantially say this due to non-availibility of considerable number of datapoints.

## Choosing alpha

```{r}
# For alpha = 5%
z_left = qnorm(0.025, mean(dataset$Duration), sd(dataset$Duration))
z_right = qnorm(0.975, mean(dataset$Duration), sd(dataset$Duration))
ggplot(dataset) +
  geom_density(aes(Duration)) +
  geom_vline(xintercept = z_left, linetype = 'dashed', color='red' ) +
  geom_vline(xintercept = z_right, linetype = 'dashed', color='red') +
  theme_bw() +
  ggtitle('Alpha for Duration') +
  theme(plot.title = element_text(hjust = 0.5))
```

Alpha defines the probability of rejecting the Null Hypothesis when it is true. Alpha has to be decided according to scenario like in the healthcare system, it is required that the analysis is accurate. In those cases we would need the alpha to be very low. For our dataset, we would be looking at alpha = 0.05.

## Hypothesis Testing

This section involves testing different hypothesis and interpreting its results.

**1) We want to test if the mean balance of our sample data is equivalent to the population mean balance.**
  
  First we take a random sample of size 1000 from the dataset by using sample_n function. 
The sample obtained will be assumed to be the true representative of the population.

#Taking sample of size 1000

```{r, message=FALSE, warning=FALSE}
set.seed(50)
balance_sample1 <- sample_n(dataset, 1000)
```

Population mean of balance is :
  ```{r, message=FALSE, warning=FALSE}
mean(dataset$Balance)
```
Sample mean of balance is :
  
  ```{r, message=FALSE, warning=FALSE}
mean(balance_sample1$Balance)
```

Let X = Random Variable of the balance in USD

Then our null hypothesis and alternate hypothesis are as below

H0: Mean Balance of the population=6.63
H1: Mean Balance of the population!=6.63

We perform z- test to test our hypothesis now. We consider our significance level at 0.05 , which means we will fail to reject the null hypothesis if the z-value lies within the range[-1.96,1.96] according to the table.
```{r, message=FALSE, warning=FALSE}
#Defining function z test with sample and population data as parameters in vector and output as z value
z.test <- function(sample, pop){
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / (sqrt(var/(n))) 
  return(z)
}

```

```{r, message=FALSE, warning=FALSE}
pnorm(z.test(balance_sample1$Balance, dataset$Balance), lower.tail = FALSE)
```



Choosing another sample of larger size and performing the z test again

```{r, message=FALSE, warning=FALSE}
set.seed(50)
balance_sample2 <- sample_n(dataset, 3000)
pnorm(z.test(balance_sample2$Balance, dataset$Balance), lower.tail = FALSE)
```

Since the z-value lies within the range [-1.96, 1.96], we thus fail to reject null hypothesis and conclude there is no significant difference between sample mean balance and population mean balance.

**2) We will test if the mean duration of the call is different for the people who take insurance than the people who don't take it.**

In order to investigate this further, we will create two subsets of the population from the dataset.

Subsetting population based on the value of car insurance

```{r, message=FALSE, warning=FALSE}
duration_0 <- filter(dataset, CarInsurance==0) #select all columns and rows where people did not take car insurance 
duration_1 <- filter(dataset, CarInsurance==1) #select all columns and rows where people have taken the car Insurance
```


Taking sample from each of the two populations created


```{r, message=FALSE, warning=FALSE}
duration_0_sample <- sample_n(duration_0, 1000) #sample 1000 rows from the first population
duration_1_sample <- sample_n(duration_1, 1000) #sample 1000 rows from the second population
```
Let
X1 = R.V. of duration of the call from first sample - duration_0_sample
X2 = R.V. of duration of the call from second sample - duration_1_sample

The mean age of sample 1 is therefore ($\mu_1$)


```{r, message=FALSE, warning=FALSE}
mean(duration_0_sample$Duration)
```


The mean age of sample 2 is therefore ($\mu_2$)
```{r, message=FALSE, warning=FALSE}
mean(duration_1_sample$Duration)
```

Thus, our null and alternate hypothesis are:
. H0: $\mu_1$ - $\mu_2$ = 0
. H1: $\mu_1$ - $\mu_2$ != 0

We will now perform the two sample z - test to test our hypothesis.

```{r, message=FALSE, warning=FALSE}

zsum.test(mean.x = mean(duration_0_sample$Duration), n.x= length(duration_0_sample$Duration),sigma.x = sd(duration_0_sample$Duration),mean.y = mean(duration_1_sample$Duration),n.y=length(duration_1_sample$Duration),sigma.y=sd(duration_1_sample$Duration),alternative = "two.sided")
```

Since p value is too less than our level of significance 0.05 , we would reject the null hypothesis and conclude that the mean duration of the call is different for the people who take insurance than the people who don't take it

Also,$\mu_1$-$\mu_2$ is negative within the 95 confidence interval which indicates that $\mu_2$ > $\mu_1$.

Hence we have significant evidence to conclude that people who take the insurance talk longer.

**3) We will test if the proportion of students taking the car insurance is different from the population proportion.**
  
  Creating the subset population where Job is Student

```{r, message=FALSE, warning=FALSE}
student_population <- filter(dataset, Job=='student')
```

Calculating the proportion of students who took the insurance in the population

```{r, message=FALSE, warning=FALSE}
student_pop_analysis <- student_population%>%
  group_by(CarInsurance)%>%
  summarise(total =n())
```

Population proportion of students taking the car insurance is  

```{r, message=FALSE, warning=FALSE}
prop_population <-student_pop_analysis$total[2]/length(student_population$Job)
prop_population
```  

In this case, our null and alternate hypothesis are:
  . H0: p = 0.68
. H1: p != 0.68

Performing test with sample size 50 

```{r, message=FALSE, warning=FALSE}
student_sample <- sample_n(student_population, size = 50)
student_sample_analysis <- student_sample%>%
  group_by(CarInsurance)%>%
  summarise(total=n())

prop.test(x=student_sample_analysis$total[2],
          length(student_sample$CarInsurance),prop_population,alternative =
            'two.sided')
```

Performing test again with sample size 100

```{r, message=FALSE, warning=FALSE}
student_sample2 <- sample_n(student_population, size = 100)
student_sample2_analysis <- student_sample2%>%
  group_by(CarInsurance)%>%
  summarise(total=n())
prop.test(x=student_sample2_analysis$total[2],
          length(student_sample2$CarInsurance),prop_population,alternative =
            'two.sided')
```

Since p value is greater than our level of significance , we fail to reject the null hypothesis and conclude that proportion of students who take the insurance is 0.68.

**4) We will test if proportion of married people who take the insurance is less that proportion of single people in the who have taken the insurance.**
  
  Let p1= proportion of married people who took the car insurance
  p2= proportion of single people who took the car insurance
  
  Thus, our null and alternate hypothesis are:
    . H0: p1 - p2 = 0
  . H1: p1 - p2 < 0
  
  We create a sample population with marital status as single and married.
  
  ```{r, message=FALSE, warning=FALSE}
  marital_sample <- filter(dataset, Marital=="single" | Marital =="married")
  ```
  
  Changing levels of factor Marital in the dataframe
  ```{r, message=FALSE, warning=FALSE}
  marital_sample$Marital <- factor(marital_sample$Marital)
  ```
  
  Choosing a sample of size 500 and performing the test of proportion
  
  ```{r, message=FALSE, warning=FALSE}
  marital_sample1 <- sample_n(marital_sample, 100)
  prop.test(table(marital_sample1$Marital, marital_sample1$CarInsurance),
            alternative = 'less', correct= FALSE)
  ```
  
  Choosing a sample of size 1000 and then performing the test again
  
  ```{r, message=FALSE, warning=FALSE}
  marital_sample2 <- sample_n(marital_sample,1000 )
  prop.test(table(marital_sample2$Marital, marital_sample2$CarInsurance), 
            alternative = 'less',correct= FALSE)
  ```
  
  Choosing a sample of size 2500 and then performing the test again
  
  ```{r, message=FALSE, warning=FALSE}
  marital_sample3 <- sample_n(marital_sample, 2500)
  prop.test(table(marital_sample3$Marital, marital_sample3$CarInsurance), 
            alternative = 'less',correct = FALSE)
  ```
  
  Since the p-value is greater than our level of significance , we fail to reject the null hypothesis and conclude that the proportion of married people who take the insurance is less than the proportion of single people who take the insurance.
  
  ## Bayes Theorem for Prediction of Outcome
  
  This section tries to device a method using to bayes theorem to try and classify if the client is going to take the insurance or not. The following code is the function to take the dataset as a input and calculate two probabilities, the probability of the client opting for a car insurance and the client not opting for a car insurance. On the basis of magnitude of the probabilities, we'll classify each record.

### Bayes Theorem

<center>
**P(A | B) = P(B | A) * P(A) / P(B)**
</center>

The above formula states the Bayes Theorem. A and B are the events and P showing the probability.

The formula given above states the bayes theorem for two events. The dataset that we have has multiple events like Age, Job, Balance, Number of Contacts and Duration. So the probability of the client taking a insurance will be probability of Insurance given the client has a particular age, job, balance, number of contacts and talks for a particular duration. Mathematically it can be stated as 

<center>
**P(I|A, B, C, D, J)** 
</center>

where I is the event for Insurance, A for Age, B for Balance, C for Number of Contacts, D for Duration and J for the job the client has.

Using the Bayes theorem we can state that
<center>
**P(I|A, B, C, D, J) = P(A, B, C, D, J | I) * P(I) / P(A, B, C, D, J)**
</center>

### How to calculate P(I | A, B, C, D, J)?

Using the joint probability, we can write P(I | A, B, C, D, J) as P(I, A, B, C, D, J). Using the chain rule we can write 
<center>
**P(I, A, B, C, D, J) = P(A | B, C, D, J, I) * P(B, C, D, J, I)**

**= P(A | B, C, D, J, I) * P(B | C, D, J, I) * P(C, D, J, I)**
</center>

By using the chain rule again we can write it as 
<center>
**P(A | B, C, D, J, I) * P(B | C, D, J, I) * ... P(J | I) * P(I)**
</center>

We now make a strong assumption on the conditional probabilities we just calculated. We assume that they are conditionally independent. In other words, knowing Balance, Job, Number of Contacts and Call Duration doesn't affect the probability of occurance of Age and the same for other variables. Mathematically it can be stated as 
  
  <center>
    **P(A | B, C, D, J, I) = P(A | I)**
    
    **P(B | C, D, J, I) = P(B | I)**
    
    **P(C | D, J, I) = P(C | I)**
    
    **P(D | J, I) = P(D | I)**
    </center>
    
    Using the above conclusions, we can state that the joint probability as 
  
  <center>
    **P(I | A, B, C, D, J) = P(A | I) * P(B | I) * P(C | I) * P(D | I) * P(J | I) * P(I)**
    </center>
    
    We'll use the above conclusion to derive a probability for a person getting a insurance and not getting a insurance and on the basis of magnitude of the probabilities, we'll classify each as a client opting for insurance and client not opting for insurance. In order to do this, firstly we'll round off the log scaled values in Balance and Duration to two decimal places.

```{r}
dataset$Balance = round(dataset$Balance, 2)
dataset$Duration = round(dataset$Duration, 2)
```

We'll define a vector to save the probabilites we derive and a filtered dataframe which has values for the clients opting for Car Insurance.
  
  ```{r}
  prob = c()
  insurance = dataset %>% filter(CarInsurance == 1)
  ```
  
  Following is the function to get the probabilties for clients opting for insurance.
  
  ```{r}
  prob_function = function(row){
    age = as.integer(row[['Age']])
    prob_age_ins = length(insurance$Age[insurance$Age %in% 
                                          (age-2):(age+2)])/nrow(insurance)
    prob_job_ins = length(insurance$Job[insurance$Job ==                                              row[['Job']]])/nrow(insurance)
    balance = as.double(row[['Balance']])
    prob_bal_ins = length(insurance$Balance[insurance$Balance
                                            %in% seq((balance-2),(balance+2), by = 0.01)])/
      nrow(insurance)
    duration = as.double(row[['Duration']])
    prob_dur_ins = length(insurance$Duration[insurance$Duration
                                             %in% seq((duration-0.7),(duration+0.7), by=0.01)])/
      nrow(insurance)
    p = prob_age_ins*prob_job_ins*prob_bal_ins*prob_dur_ins
    p_insurance = nrow(insurance)/nrow(dataset)
    old_probs <<- prob
    prob <<- c(old_probs, (p*p_insurance))
    return(p*p_insurance)
  }
  ```
  
  Calling the function for the dataset that we have.
  
  ```{r, echo=FALSE, results='hide'}
  apply(dataset, 1, prob_function)
  ```
  
  Calling the function will store the values in the prob vector. These are values for a positive result. In other words, a person opting to get the insurance. Now we need to calculate the probability for a person opting out of the car insurance. For accomplishing this we'll call the function again.

```{r, message==FALSE, results='hide'}
pos_prob = prob
prob = c()
insurance = dataset %>% filter(CarInsurance == 0)
apply(dataset, 1, prob_function)
neg_prob = prob
```

No we have two sets of probabilties. One for a client opting for insurance and the other for client opting out of the insurance. Now on the basis of magnitude we'll identify the outcome of the event.
  
  ```{r}
  result = ifelse(pos_prob > neg_prob, 1, 0)
  ```
  
  Checking the obtained values against the values which are already present in the dataset.
  
  ```{r}
  table(dataset$CarInsurance, result)
  ```
  
  This shows in total 2452 records were correctly identified by the function we just wrote out of the 3250 records! We'll use the e1071 library to run the naive bayes classifier and compare our results with it. 

```{r}
dataset$CarInsurance = factor(dataset$CarInsurance, levels = c(0,1), labels = c(0,1))
classifier = naiveBayes(x = dataset[c(2, 3, 7, 13, 20)],
                        y = dataset$CarInsurance)
pred = predict(classifier, dataset[c(2, 3, 7, 13, 20)])
```

Comparing the results with the dataset actual values.

```{r}
table(dataset$CarInsurance, pred)
```

Naive bayes classifier could identify in total 2420 values correctly. The results show that the function we wrote performed better than the naive bayes classifier on the same dataset to identify the cutomers opting out of the insurance. This was a basic implementation of how naive bayes theorem can be used to accomplish classification problem.

## References

1. Kaggle
2. https://blog.easysol.net/machine-learning-algorithms-4/ - Naive Bayes 
   Explaination
3. Probability and Statistics for Engineers and Scientists - 9th Edition
4. https://cran.r-project.org/ - R functions
