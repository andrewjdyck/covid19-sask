# Forecasting COVID-19 in Saskatchewan

Uses a simple logistic model to predict COVID-19 cases in Saskatchewan, Canada.

![Sask COVID-19 forecasted cases over time](forecast.gif)

### Assumptions

This is going to be a bit on the dark side. Until there is a fatality in SK, some predictive analysis is a bit limited.....I guess that's a good position to be in. That said, without this data, I'm going to use some Canadian averages. For example, I'll use an estimate of the Canadian detection rate of 6% of COVID-19 cases making their way into the official stats. 

I'm also going to assume that any social distancing measures take 14 days to be effective and, like others, assume that the median time from infection to symptoms is 5 days. Also like others, the time from infection to fatality is assumed to be 17 days.

Unlike others, I'm going to use a less conservative estimate of the case fatality rate (CFR) for COVID-19, that is based on as close as we have to an experiment, the Diamond Princess, which was 1%. Some notes on this come from: https://www.statnews.com/2020/03/17/a-fiasco-in-the-making-as-the-coronavirus-pandemic-takes-hold-we-are-making-decisions-without-reliable-data/, and, a very trustworthy source, https://www.nature.com/articles/d41586-020-00885-w.
