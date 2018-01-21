#这个是翻译版本#

---
title: "Exploratory Analysis Zillow"
output:
  html_document:
    fig_height: 4
    fig_width: 7
    theme: cosmo
---

### Welcome and good luck to you all at Zillow's Home Value prediction!
#### with a price pool of 1.2 Million Dollar

Here is a first exploratory analysis of the competition dataset.
We are provided with a list of real estate properties in three counties (Los Angeles, Orange and Ventura, California) data in 2016.
#这是对竞争数据集的第一次探索性分析。
#我们提供了2016年3个县(洛杉矶，橙和文图拉，加州)的房地产项目清单。

Zillow provides a "Zestimate", which is an estimated property value.
#Zillow提供了一个“Zestimate”，这是一个     估计      的属性值。

Our task in this competition is to predict the the difference between the actual price and the estimate of the price (Zestimate). So, in fact we are predicting, where Zillow's Zestimate will be good, and where it will be bad.
#我们在这次竞赛中的任务是预测实际价格与价格的估计之间的差额(Zestimate)。                                                          所以，事实上我们在预测，Zillow的z估计值会很好，而且会很糟糕。

So, far so good. However, we don't have to predict a single value, but instead for 6 different time points (from October 2016 to December 2017) (see sample submission)
#因此,很好。     但是，我们不需要预测一个值，而是选择6个不同的时间点(从2016年10月到2017年12月)(见样本提交)

The dataset consists of information about 2.9 million properties and is grouped into 2 files:
#该数据集包含大约290万个属性，并被分成两个文件:
  - properties_2016.csv (containing information about the properties themselves) #包含有关属性本身的信息。
  - train_2016.csv (containing information about the transcations)  #包含关于销售的信息。
  
Let's look at the dataset:

### Read in the data
```{r, message=FALSE, warning=FALSE, results='hide'}
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
 
properties <- fread('C:/data/properties_2016.csv')           #房屋属性
transactions <- fread('C:/data/train_2016_v2.csv')           #销售信息
sample_submission <- fread('C:/data/sample_submission.csv')  #样本提交

```


```{r include=FALSE}
#设置全局参数
options(tibble.width = Inf)
```


Lets first have a look at these files:

### Peek at the dataset {.tabset}

#### Properties
There is a total of 58 features per property. You can scroll the x-axis to see all features.
#每个属性共有58个特性。您可以滚动x轴以查看所有特性。


```{r, result='asis', echo=FALSE}
datatable(head(properties,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```
#datatable 是查看数据的一种方法 如果数据太大 可以用这种方法查看


#### Transactions

```{r, result='asis', echo=FALSE}
datatable(head(transactions,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))
```


#### Sample Submission

```{r, result='asis', echo=FALSE}
datatable(head(sample_submission,10), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))

```


### Renaming the features
The feature names are (lets face it) not really interpretable well. How should you now what "finishedsquarefeet15" is for example. So, I am going to rename them here. This will make working with the dataset a lot easier (more consistency, and shorter names):
#特性名称(让我们面对它)并不是很好解释。                           例如，你怎么知道“finishedsquarefeet15”代表什么。                      我将在这里重命名它们。           这将使处理数据集更容易(更一致，更短的名称):

```{r warning=FALSE, message=FALSE}
#字段重命名  将复杂的英文变为易读的
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)
transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

#对tax_delinquency、flag_fireplace、flag_tub字段  进行特征编码 
#mutate() adds new variables and preserves existing
properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))
```

### Peek at the renamed dataset {.tabset}
#查看重命名的数据集。

#### Properties
Much better :-). You can scroll滚动 the x-axis to see all features. 
```{r, result='asis', echo=FALSE}
properties <- properties %>% select(id_parcel, build_year, starts_with("area_"), starts_with("num_"), starts_with("flag_"), starts_with("region_"), everything())
datatable(head(properties,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

#### Transactions

```{r, result='asis', echo=FALSE}
datatable(head(transactions,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))
```

### Transaction Dates
This is a little bit confusing at first glance: There are three different time slots for transactions. So I made a figure:
![](http://i.imgur.com/OibqfFb.png)
#乍一看，这有点令人困惑:交易有三个不同的时间段。所以我做了一个图表: http://i.imgur.com/OibqfFb.png 

#### Distribution of transaction dates 
#分布的交易日期
As shown in the figure above, there are only some of the transactions after 25.10 in the train set, because the rest is in the test set (for the public LB).
#如上面的图所示，有一些销售事件在训练集的10月25号之后，因为其余的都在测试集中(对于公共LB)。
#就是10月25之前的作为训练集 用来训练模型  10月25之后的用来测试

```{r}
#tmp是将transactions的date进行了转换 但是根据转换结果来看 好像并没有什么变化  根据销售月份进行统计
tmp <- transactions %>% mutate(year_month = make_date(year=year(date),month=month(date)))
#用临时变量tmp画图  %>% 管道    
#group_by(year_month) 根据年月分组   
#count()计数  
#ggplot(aes(x=year_month,y=n))  画图 x轴year_month  y轴n
#geom_bar   画柱状图 
#geom_vline 画参考线
tmp %>% 
  group_by(year_month) %>% count() %>%    
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="red")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01"))),size=2)
```


### Outcome
To get a feel for the data let's first have a look at the distribution of our outcome (logerror), i.e. the difference in log(Zestimate)-log(Saleprice)
#为了获得数据的大概情况，让我们先看看结果的分布(logerror)， 即log(Zestimate)-log(Saleprice)的差异

#geom_histogram 将连续数值转化为直方图 bins表示长方体宽度
#theme_bw()
#theme
#coord_cartesian
```{r warning=FALSE}
transactions %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))
```

### More thoughts about the outcome
In fact there are two outcomes you can look at:  

  - logerror: log(Zestimate) - log(Saleprice). So a positive logerror means Zestimate is overestimating the Saleprice, a negative logerror means that Zestimate is underestimating Saleprice. 
  - absolute logerror: a small value means that log(Zestimate) is close to log(Saleprice). So, Zestimate predictions are close to Saleprice.
#因此，一个正的logerror意味着Zestimate高估了Saleprice，一个负的logerror意味着Zestimate低估了Saleprice。
#一个小的值意味着log(Zestimate)接近于log(Saleprice)。因此，Zestimate的预测接近于Saleprice。

Any association with logerror would indicate that a feature would be associated with over- or understimating低估 the sale price.
Any association of a feature with absolute logerror would indicate that the feature is associated with a better or worse Zestimate.
#logerror的关系表明，一个特性将与销售价格相关联。
#任何带有绝对值logerror的特性的关联都表明该特性与一个更好的或更差的z估计值有关

### Absolute logerror
```{r}
#增加了一列  绝对值abs_logerror
transactions <- transactions %>% mutate(abs_logerror = abs(logerror))
transactions %>% 
  ggplot(aes(x=abs_logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(0,0.5))
```

### How does absolute log error change with time
There seems to be a light trend that Zestimates error decreases over time. Predictions are getting better. Zillow seems to have good data scientists :-).
#随着时间的推移，z估计值的误差会逐渐减小。预测是越来越好。Zillow似乎有很好的数据科学家:-)。
```{r}
transactions %>% 
  mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
  group_by(year_month) %>% summarize(mean_abs_logerror = mean(abs_logerror)) %>% 
  ggplot(aes(x=year_month,y=mean_abs_logerror)) + 
  geom_line(size=1.5, color="red")+
  geom_point(size=5, color="red")+theme_bw()
```

### How does log error change with time
```{r}
transactions %>% 
  mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
  group_by(year_month) %>% summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=year_month,y=mean_logerror)) + 
  geom_line(size=1.5, color="red")+geom_point(size=5, color="red")+theme_bw()
```

### Missing values
We have seen many missing values in the data peeking. 
How many missing values are there for each feature?
In fact, some features are missing nearly completely. So, we probably have to work more with the others. 
#我们在数据中看到了许多缺失的值。
#每个特征有多少缺失值?
#事实上，有些特征几乎完全缺失。所以，我们可能需要更多地与其他特征合作。
#缺失值转换

```{r fig.height=15, warning=FALSE}
#funs() provides a flexible way to generate a named list of functions for input to other functions like summarise_at().
#funs() 貌似是一个函数列表
missing_values <- properties %>% summarize_each(funs(sum(is.na(.))/n()))   #统计缺失值
#Gather takes multiple columns and collapses into key-value pairs, duplicating all other columns as needed. You use gather() when you notice that you have columns that are not variables.
#gather 把missing_values 行转列  变成key-value形式
missing_values <- gather(missing_values, key="feature", value="missing_pct") 
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()
#筛选 pct<0.75作为有效特征
good_features <- filter(missing_values, missing_pct<0.75)
```


### Correlation with absolute logerror
num_ features:

```{r, fig.height=5}
#str_detect    矢量化字符串  把good_features$feature里面num提取出来
vars <- good_features$feature[str_detect(good_features$feature,'num_')]

cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))

corrplot(cor(tmp, use="complete.obs"),type="lower")

```

### Correlation with absolute logerror
#绝对值相关性
area_features #面积特征

```{r, fig.height=5}
#detect 检测
vars <- good_features$feature[str_detect(good_features$feature,'area_')]

tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
#corrplot画相关图
corrplot(cor(tmp, use="complete.obs"), type="lower")

```


### Correlation with absolute logerror
tax_features  #税特征

```{r, fig.height=5}
vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))

tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")

```

### Correlation with logerror
num_ features:  #数量特征
There seems to be small negative correlations between the num features and logerror.
#在num特性和logerror之间似乎存在小的负相关。

```{r, fig.height=5}
vars <- good_features$feature[str_detect(good_features$feature,'num_')]

cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"logerror")))

corrplot(cor(tmp, use="complete.obs"),type="lower")

```

### Correlation with logerror
#没有绝对值的相关性

area_ features

```{r, fig.height=5}
vars <- good_features$feature[str_detect(good_features$feature,'area_')]

tmp <- cor_tmp %>%  select(one_of(c(vars,"logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")

```

### Correlation with logerror
tax_ features

```{r, fig.height=5}
vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))

tmp <- cor_tmp %>%  select(one_of(c(vars,"logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")

```

Overall, correlations with log error are quite small. This is a hint that predictions are quite good already. However, we might still find some features or at least ranges of feature values where predictions can still be improved. 
#总的来说，log error的相关性很小。这暗示着预测已经相当不错了。但是，我们仍然可以找到一些特性，或者至少是一些特性值的范围，这些值的预测仍然可以得到改进。

### When were the houses built?
#这些房子是什么时候建造的?

Let's plot the distribution of build year for the houses. Most houses were built around 1950. There are not many older houses, neither many new houses >2000.
#我们来画一下房屋建筑年的分布图。大多数房屋建于1950年左右。没有很多老房子，也没有很多新房子是2000年以后建造的
```{r, warning=FALSE}
cor_tmp %>% 
  ggplot(aes(x=build_year))+geom_line(stat="density", color="red", size=1.2)+theme_bw()
```

### How does the absolute logerror change with build_year?
#绝对值logerror是如何随着build_year变化而变化的?
Predictions are better for newer houses. As we saw in the figure above we have way fewer older houses. However, what is interesting is that there are many houses with build year around 1950, but predictions for those houses are not too good. 
#对新房子的预测比较好。正如我们在上图所看到的，我们的老房子越来越少了。然而，有趣的是，在1950年前后有很多房子，但对这些房子的预测并不太好。

```{r, warning=FALSE, message=FALSE}
cor_tmp %>% 
  group_by(build_year) %>% 
  summarize(mean_abs_logerror = mean(abs(logerror)),n()) %>% 
  ggplot(aes(x=build_year,y=mean_abs_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.25))+theme_bw()
```


### How does the logerror change with build_year?

```{r, warning=FALSE}
cor_tmp %>% 
  group_by(build_year) %>% 
  summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=build_year,y=mean_logerror))+
  geom_smooth(color="grey40")+
  geom_point(color="red")+coord_cartesian(ylim=c(0,0.075))+theme_bw()
```

### Where does Zestimate predict well?
#Zestimate在哪里预测的好?
To get a quick feel where zestimate predicts well, we can group our absolute logerror into different percentiles, e.g. the percentile with best predictions (top 10%), worst predictions (worst 10%) and typical predictions (50% around the median).
#为了快速了解zestimate预测的情况，我们可以将绝对误差划分成不同的百分比，例如:最佳预测(前10%)、最差预测(最差10%)和典型预测(中间值的50%)。

```{r, warning=FALSE}

transactions <- transactions %>% mutate(percentile = cut(abs_logerror,quantile(abs_logerror, probs=c(0, 0.1, 0.25, 0.75, 0.9, 1),names = FALSE),include.lowest = TRUE,labels=FALSE))
#根据误差划分预测的好坏
tmp1 <- transactions %>% 
  filter(percentile == 1) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp2 <- transactions %>% 
  filter(percentile == 5) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp3 <- transactions %>% 
  filter(percentile == 3) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")

tmp1 <- tmp1 %>% mutate(type="best_fit")
tmp2 <- tmp2 %>% mutate(type="worst_fit")
tmp3 <- tmp3 %>% mutate(type="typical_fit")


tmp <- bind_rows(tmp1,tmp2,tmp3)
tmp <- tmp %>% mutate(type = factor(type,levels = c("worst_fit", "typical_fit", "best_fit")))
```

If the distributions of features are largely overlapping for these three groups of transactions the feature most likely does not have a large effect on the goodness of estimation. 
Let's see one example.
#如果这些特性的分布在这三组事务上有很大的重叠，那么这个特性很可能不会对评估的好处有很大的影响。
#让我们看一个例子。

```{r, warning=FALSE, message=FALSE}
col_pal <- "Set1"

tmp %>% ggplot(aes(x=latitude, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

```

We can see that rows resulting in the worst predictions have a lower density for lower latitude values, but a higher density for intermediate latitudes (around 34000000). 
#我们可以看到，导致最差预测的行密度较低，但中纬度地区的密度更高(约34000000)。

We can examine this effect more closely and plot the absolute logerror as a function of latitude.
#我们可以更仔细地研究这种效应，并将绝对误差作为纬度的函数。

```{r, warning=FALSE, message=FALSE, fig.width=6}
tmptrans <- transactions %>% 
  left_join(properties, by="id_parcel")

tmptrans %>% 
  ggplot(aes(x=latitude,y=abs_logerror))+geom_smooth(color="red")+theme_bw()
  
```

Having seen the example, we can look at other features quickly, to see which are associated with absolute logerror.
#在看到这个示例之后，我们可以快速查看其他特性，以查看与绝对logerror相关的特性。
#感觉这个例子有点像数据挖掘，就是已经有了预测结果，通过比较预测结果和实际结果之间的误差，比较那个字段会影响预测结果的好坏
```{r, warning=FALSE, echo=FALSE, message=FALSE}

tmp %>% ggplot(aes(x=longitude, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

```

```{r, warning=FALSE, fig.width=6, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=longitude,y=abs_logerror))+geom_smooth(color="red")+theme_bw()
```

```{r, warning=FALSE, echo=FALSE, message=FALSE}
tmp %>% ggplot(aes(x=area_total_finished, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(700,7.5e3))
```

```{r, warning=FALSE, fig.width=6, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=area_total_finished,y=abs_logerror))+geom_smooth(color="red")+theme_bw()+ coord_cartesian(xlim=c(600,7.5e3),ylim=c(0.1,0.2))
```

```{r, warning=FALSE, echo=FALSE, message=FALSE}

tmp %>% ggplot(aes(x=area_live_finished, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,1e4))

tmp %>% ggplot(aes(x=num_room, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,10))

tmp %>% ggplot(aes(x=num_unit, fill=type, color=type)) + geom_line(stat="density", size=1.2) + 
theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(1,4))

tmp %>% ggplot(aes(x=build_year, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

tmp %>% ggplot(aes(x=tax_total, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,1e6))
```

```{r, warning=FALSE, fig.width=6, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=tax_total,y=abs_logerror))+geom_smooth(color="red")+theme_bw()+ coord_cartesian(xlim=c(0,1e6),ylim=c(0.05,0.2))
```

```{r warning=FALSE, message=FALSE, echo=FALSE}

tmp %>% ggplot(aes(x=tax_building, fill=type, color=type)) + geom_line(stat="density", size=1.2) + theme_bw()+scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)+ coord_cartesian(xlim=c(0,1e6))

```



### Where does Zestimate over or underpredict?
#Zestimate在什么地方高估或者低估
A high absolute logerror tells us predictions are not that good. But this could either be driven by underpredicting or overpredicting sale price. 
#高绝对误差告诉我们，预测并不是那么好。但这可能是由于低估或高估了销售价格。

```{r}
tmptrans <- tmptrans %>% mutate(overunder = ifelse(logerror<0,"under","over"))
```

Again, we see that there is no clear correlation. However, for some range in the data Zestimate tends to overpredict more than to underpredict (see where the red line is above the blue one).
#再一次，我们看到没有明确的相关性。然而，在数据的某些范围内，z估计数倾向于高估而不是低估(看红线在蓝色线上的位置)。

```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=latitude,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")
```

```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=longitude,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")
```

Both for latitude and longitude there is a range where Zestimate both under- and overpredicts. 
#在纬度和经度上，都有一个范围， Zestimate的高估和低估预测值都在这个范围内。
Where is that?

```{r}
leaflet() %>% 
  addTiles() %>% 
  fitBounds(-118.5,33.8,-118.25,34.15) %>% 
  addRectangles(-118.5,33.8,-118.25,34.15) %>% 
  addMiniMap()
```
  
For properties with small calculated total area, Zestimate seems to overpredict.
#对于计算面积较小的性质，Zestimate似乎预测过高。
```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% filter(area_total_calc <=5000) %>% ggplot(aes(x=area_total_calc,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")
```


Whereas for actual finished area there is no such effect. 
#而对于实际完工区域则没有这样的效果。

```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=area_total_finished,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw()+coord_cartesian(xlim=c(500,7.5e+03),ylim=c(0,0.3))+scale_color_brewer(palette="Set1")
```

```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% filter(area_lot <=1e+05) %>% ggplot(aes(x=area_lot,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")
```

```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=num_room,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")
```

```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=build_year,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw()+scale_color_brewer(palette="Set1")
```
```{r, warning=FALSE, fig.width=5.5, message=FALSE, echo=FALSE}
tmptrans %>% ggplot(aes(x=tax_total,y=abs(logerror), color=overunder))+geom_smooth()+theme_bw() + coord_cartesian(xlim=c(0,1e+06),ylim=c(0,0.2))+scale_color_brewer(palette="Set1")
```


### Where are all those properties?
#这些属性在哪里?
Show 2,000 of the properties on the map.
```{r , message=FALSE, warning=FALSE, fig.height=7}
lat <- range(properties$latitude/1e06,na.rm=T)
lon <- range(properties$longitude/1e06,na.rm=T)

tmp <- properties %>% 
  sample_n(2000) %>% 
  select(id_parcel,longitude,latitude) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon) %>% 
  left_join(transactions,by="id_parcel")

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircleMarkers(stroke=FALSE) %>% 
  addMiniMap()

```

### Map absolute logerror
Show the absolute logerror on map. Red = higher.
#在地图上显示绝对误差。红色=更高。

```{r warning=FALSE, message=FALSE, fig.height=7}

tmp <- transactions %>% 
  sample_n(2000) %>% 
  left_join(properties,by="id_parcel") %>% 
  select(id_parcel,longitude,latitude, abs_logerror) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon, abs_logerror)

qpal <- colorQuantile("YlOrRd", tmp$abs_logerror, n = 7)

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircleMarkers(stroke=FALSE, color=~qpal(abs_logerror),fillOpacity = 1) %>% 
  addLegend("bottomright", pal = qpal, values = ~abs_logerror,title = "Absolute logerror",opacity = 1) %>% 
    addMiniMap()

```



### What do these features mean?
#这些特征意味着什么?

I was really unsure about the meaning of the variables: 
#我不知道这个字段是啥意思 维基百科搜索到下面内容
  - 'rawcensustractandblock'

A wikipedia search revealed:

Census tracts represent the smallest territorial unit for which population data are available in many countries.[3] In the United States, census tracts are subdivided into block groups and census blocks. In the U.S., census tracts are "designed to be relatively homogeneous units with respect to population characteristics, economic status, and living conditions" and "average about 4,000 inhabitants".

```{r}
str(properties$rawcensustractandblock[1])
```
Looks like a number. However a closer look reveals its actually a number composed of two parts, separated by a dot.
#看起来像一个数字。然而，仔细一看，它实际上是一个由两个部分组成的数字，由一个点分开。
```{r}
as.character(properties$rawcensustractandblock[1])
```
Ok, so far so good. So from what I read this number consisits of:
#到目前为止一切顺利。所以从我读到的数字来看
FIPS Code (6037) - Tract Number (8002.04) - And block Number (1)

Let's specifically add the tract number and block number information
#让我们具体地添加通道数和块号信息。
```{r}
properties <- properties %>% mutate(census = as.character(rawcensustractandblock), tract_number = str_sub(census,5,11), tract_block = str_sub(census,12))
```

**An important note:**  
For round 1, the use of external data is not permitted. So save the pleasent anticipation for round 2 :-)
#对于第1轮，不允许使用外部数据。因此，请保留对第二轮的愉悦期待:-)
FIPS codes can be looked up [here](https://www.ffiec.gov/census/Default.aspx)

  - 6037 Los Angeles
  - 6059 Orange County
  - 6111 Ventura County

Now, that you have the tract number you can get information such as tract income level, median family income, tract population, etc.
#现在，你可以得到你可以得到的信息例如，收入水平，家庭收入中位数，人口数量，等等。

For our example "6037 8002.04" lets look up some information:

https://www.ffiec.gov/census/report.aspx?year=2016&county=037&tract=8002.04&state=06&report=demographic

For example: The median familiy income 2016 is $146,472. 




<br><br>

I am going to add more information later. So stay tuned.

And:

If the kernel helped you and you **upvote**, you make me happy :-)
