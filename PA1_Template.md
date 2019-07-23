title: "Peer-graded Assignment - First Course Project"
author: "Jonah Winninghoff"
date: "7/22/2019"

This paper develops based on Peer-graded Assignment: Course Project 1 that will evaluate my ability to manipulate dataset and create a set of plots and histograms. Not only does it evaluates on my ability, it evaluates my ability to use *Markdown* so that it intends to cover:

1.) Coding for reading in the dataset

2.) Histogram of the total of steps per day  

3.) Mean and median number of steps per day  

4.) Plotting timeline of the average number of steps  

5.) 5-minute interval containing the maximum number of steps on average  

6.) Code to describe and show how to impute missing data  

7.) Post-missing removal histogram of the total number of steps per day  

8.) Panel plot in comparsion to weekend and weekday average number of steps per 5-minute interval

From now on, the first part is to demonstrate how to code for reading in the dataset. To identify my getwd() and organize my file both are needed, of course. When this pre-reading process is done, the coding for reading this file is not only a thing needed to do but ggplot2, dplyr are applied. Also, the *date* in dataset is coerced into date and dataset is organized for convenient reading. Please look here:  

```{r echo = TRUE}
paste(getwd(), "/activity.csv", sep = "") -> reading

read.csv(reading) -> Q1

library(ggplot2)

library(dplyr)

as.Date(Q1$date) -> Q1$date

tbl_df(Q1) -> Q1

Q1
```

The next part is to develop histogram to determine the total of steps per day, which is, admittedly, little challenging because this formula is quite new. Despite this, the ggplot2 and dplyr both are in use to group and summary of summing steps from Q1 dataset and then develop the histogram.

```{r echo = TRUE}
Q1 %>% group_by(date) %>% summarise(Plot1 = sum(steps, na.rm = TRUE)) %>% 
  ggplot(aes(x = Plot1)) + geom_histogram(binwidth = 806, 
  na.rm = TRUE, color = "white") + ggtitle(
  "Histogram: Total Number of Steps Per Day") + xlab(
  "Total steps per day") + 
  ylab("Frequency") + theme_dark(base_family = "Times")

```

The third part is to find mean and median of Q1 steps, which might be an easiest part to code.

```{r echo=TRUE}
Q1 %>% group_by(date) %>%
  summarise(mean(steps, na.rm = TRUE))

summarise(Q1, mean(steps, na.rm = TRUE))

Q1 %>% group_by(date) %>%
  summarise(median(steps, na.rm = TRUE))

summarise(Q1, median(steps, na.rm = TRUE))
```

The fourth part is the plotting timeline of the average number of steps. In case if the disturbing warning indicates, it means that NA missing causes to create broken linear graph, which should be this way. To make graph smooth is possible by coding na.omit() at the beginning but since the instruction explains that NA missing should replace with mean of steps, this graph should not change.

```{r echo=TRUE}
Q1 %>% group_by(date) %>% summarise(Plot2 = 
  mean(steps, na.rm = TRUE)) %>% ggplot(aes(
  x = date, y = Plot2))+ geom_line(color = "white") + labs(
  title = "Average Number of steps per day", 
  x = "Date", y = "Mean Steps") + theme_dark(base_family = 
  "Times")
```

The fifth part is to find the maximum number of steps from within 5 minute interval through all of this dataset, which is fairly with simple coding.

```{r echo=TRUE}
Q1 %>% group_by(interval) %>% summarise(P = mean(steps, 
  na.rm = TRUE)) %>% filter(P == max(P, na.rm = TRUE))
```

As mentioned earlier, the instruction expects me to develop a strategy of coding the replacement for NA missing. This process requires a lot of thoughts and it is time-consuming but at least, it is successful.

```{r echo=TRUE}
# Locate the column containing with NA missing
Q1$steps %>% is.na() %>% as.numeric() %>% sum()
Q1$date %>% is.na() %>% as.numeric() %>% sum()
Q1$interval %>% is.na() %>% as.numeric() %>% sum()
# Substitute NA with mean of steps on steps column only
arrange(Q1, steps) -> P
select(P, c(date, interval)) -> Keeper
select(P, steps) -> Editing
slice(Editing, 1:15264) -> p
x <- .001
while(x < 656700.2){
  add_row(p, steps = 37.3826) -> p
  as.numeric(lapply(p, sum)) -> x
}
cbind(p, Keeper) -> Q2
tbl_df(Q2) -> Q2
# The result shows that sum of NA missing is zero
Q2$steps %>% is.na() %>% as.numeric() %>% sum()
```

The seventh part is to develop histogram to determine the total of steps per day after the NA missing replaces. Also, the mean and median of steps display below this histogram because this information is necessary to reveal the change after NA missing replaces. Some changes are conspicuous, say, that in histogram value of frequency significantly declines in first few steps but significantly increases between 10000 and 12000 steps.

```{r echo=TRUE}
Q2 %>% group_by(date) %>% summarise(Plot1 = sum(steps)) %>% 
  ggplot(aes(x = Plot1)) + geom_histogram(binwidth = 806, color = 
  "white") + ggtitle(
  "Histogram: Total Number of Steps Per Day") + xlab(
  "Total steps per day") + 
  ylab("Frequency") + theme_dark(base_family = "Times")

Q2 %>% group_by(date) %>%
  summarise(mean(steps))

summarise(Q2, mean(steps))

Q2 %>% group_by(date) %>%
  summarise(median(steps))

summarise(Q2, median(steps))
```

The final part is probably the one of most difficult things to code. It requires of me to install package named gridExtra, which allows to give more options by assembling two different plots into a panel. But at least, this coding is successful after many failures. The purpose of this panel is to find comparison between weekend and weekday based on average of steps at each interval.

```{r echo=TRUE}
library(gridExtra)
Q2 -> Q3
Week <- sapply(Q3$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})
mutate(Q3, Week) -> Q3
filter(Q3, Week == "Weekend") -> Weekend
filter(Q3, Week == "Weekday") -> Weekday

Weekday %>% group_by(interval) %>% summarise(Plot2 = 
  mean(steps)) %>% ggplot(aes(x = interval, y = Plot2)
  )+ geom_line(color = "cadetblue1") + labs(
  title = "Weekday", 
  x = "Interval", y = "Mean Steps") + theme_dark(base_family = 
  "Times") -> Pr
Weekend %>% group_by(interval) %>% summarise(Plot2 = 
 mean(steps)) %>% ggplot(aes(x = interval, y = Plot2)
 )+ geom_line(color = "chartreuse") + labs(
 title = "Weekend", 
 x = "Interval", y = "Mean Steps") + theme_dark(base_family = 
 "Times") -> pR
grid.arrange(pR, Pr, nrow=2)
```

Thank you for taking your time to read this.
