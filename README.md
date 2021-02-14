
---
title: "<center> Digital Media Ad Campaign</center>"
subtitle: "<center>Performance Analysis & Recommendations</center>"
author: "<center> Kimberly Healy  |  healy.kim@gmx.us </center>"
output: 
  html_document:
  toc: true
toc_depth: 2

knit: (function(input_file, encoding) {
  out_dir <- 'docs';
  rmarkdown::render(input_file,
 encoding=encoding,
 output_file=file.path(dirname(input_file), out_dir, 'index.html'))})

---
  
## Background
  The data (DigitalMediaCampaign.xlsx) come from a digital campaign that included banners, digital video, and rich media advertisements. This report provides insight into the campaign performance.  The variables in the raw data set include:   
    
     
Variable | Type | Meaning | Example
---------|------|---------|--------
Creative Name | string | name of an individual ad creative | Product_KSP1_200off_BAN   
Placement: Site_Dimensions_CreativeType_Device_CostMethod_Web/App_ID | string | a collection of seven attributes | Site A_160 x 600_BAN_DT_CPM_A_PPJHJH   
Date | date | date ad ran | 4/25/18   
Impressions | numeric | number of times an ad was presented on screen to a viewer   
Clicks | numeric | number of times an ad was clicked    
25% Viewed | numeric | number of people who watched 25% of a video or rich media ad   
50% Viewed | numeric | number of people who watched 50% of a video or rich media ad   
75% Viewed | numeric | number of people who watched 75% of a video or rich media ad   
100% Viewed | numeric | number of people who watched 100% of a video or rich media ad   
Video Replay | numeric | number of times a rich media video was replayed    
Interactions | numeric | number of people who interacted with a rich media ad   
   
   
   

The following KPIs were investigated:    
  - **Clicks**: The number of times the ad is clicked     
- **Click-Thru-Rate**: The rate which impressions drive clicks (Clicks / Impressions)   
- **Completion Rate**: The rate which video impressions are viewed (Completes / Impressions)    
- **Engagement Rate**: The rate which rich media is engaged with (Interactions / Impressions)       
   
   
The following questions were answered:     
  - **How does the performance of the ads differ by Site, Device, or Creative Name?** (PARTS V-VII)       
- **Are there any trends in KPI performance over time?** (PART VIII)       
- **What recommendations would you make? What other KPIs would you consider?** (PART IX)     
   
   
***
  
## I. Load required libraries 
```{r lib, message = FALSE}
library(tidyverse)
library(formattable)
```
***
## II. Load raw data from Excel file

``` {r load}
banner_raw <- readxl::read_excel("DigitalMediaCampaign.xlsx", sheet = 1)
video_raw <- readxl::read_excel("DigitalMediaCampaign.xlsx", sheet = 2) %>%
  rename(Impressions = `#Impressions`)
rich_raw <- readxl::read_excel("DigitalMediaCampaign.xlsx", sheet = 3)
```

***
## III. Define Useful Functions


### (i) Split Placement Column

``` {r fun1}
split_placement <- function(df){
  df%>%
    separate(`Placement: Site_Dimensions_CreativeType_Device_CostMethod_Web/App_ID`,
             c("Site", "Dimensions", "Creative Type", "Device", "Cost Method", "Web/App", "ID"),
             sep = "_",
             remove = FALSE)%>%
    select(-`Placement: Site_Dimensions_CreativeType_Device_CostMethod_Web/App_ID`)%>%
    mutate(across(where(is.character), as.factor))
}
```


### (ii) Calculate KPIs
``` {r fun2}
calc_CTR <- function(df){
  df%>%
    mutate(CTR = percent(Clicks/Impressions))
}

calc_Eng_Rate <- function(df){
  df%>%
    mutate(Eng_Rate = percent(Interactions/Impressions))
}

calc_Comp_Rate <- function(df){
  df%>%
    mutate(Comp_Rate = percent(`100% viewed`/Impressions))
}
```

***
## IV. Transform Raw Data
``` {r transf}
banner_split <- split_placement(banner_raw)
video_split <- split_placement(video_raw)
rich_split <- split_placement(rich_raw)%>%filter(Date != as.Date("2018-04-25"))
```
***

## V. Performance Evaluation - Site

### (i) Banner
``` {r perform}
banner_by_Site <- banner_split%>%
  group_by(Site)%>%
  summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))%>%
  calc_CTR()%>%
  arrange(desc(CTR))

banner_by_Site %>% formattable(align = c("l",rep("r", ncol(banner_by_Site) - 1)), 
                               list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                                    `CTR` = color_tile("#DeF7E9", "#71CA97")
                               )) 
```

The metrics for Site F are completely different from the other Sites. There are two courses of action:    
  (a) Exclude Site F from analysis    
(b) Switch the values of Clicks and Impressions for Site F as it appears that Clicks and Impressions have been transposed.  
Let's make an assumption and take course (b): retain the data and transpose Clicks and Impressions for Site F in the data.   

```{r bannerF}
banner_split <- banner_split %>% transform(Clicks = ifelse(Site == "Site F", Impressions, Clicks), 
                 Impressions = ifelse(Site == "Site F", Clicks, Impressions))

names(banner_split)[names(banner_split) == "Creative.Name"] <- "Creative Name"
names(banner_split)[names(banner_split) == "Creative.Type"] <- "Creative Type"


banner_by_Site <- banner_split%>%
  group_by(Site)%>%
  summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))%>%
  calc_CTR()%>%
  arrange(desc(CTR))

banner_by_Site %>% formattable(align = c("l",rep("r", ncol(banner_by_Site) - 1)), 
            list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97")
                )) 
```

Banners do best on Sites J, C, and M based on CTR. According to CMO Michael Brenner, the average CTR for Banners is 0.06%*. 12 out of the 17 Sites have average or above-average CTR for this type of advertising.   
   
There is a higher correlation between Impressions and Clicks on Banner ads (r = 0.519) than Video (r = 0.128) or Rich Media (r = 0.432). However, it has been reported that around 60% of clicks on banner ads are accidental*.   
   
There is no way to measure accidental clicks with the given data, but we can entertain the idea that the intentional CTR is lower than the chart indicates for Banners. Even though the CTR may not necessarily reflect accurate numbers, they're still useful if you assume that the accidental clicking effect is constant among all Sites.    

**Recommendation:**   
  **(R1)** To distinguish between accidental and intentional ad clicking, Bounce Rate and Exit Rate should be calculated. Data required: total one-page visits, total entrance visits, total exits from page, total visits to page.    

### (ii) Video
```{r vid}
video_by_Site <- video_split%>%
  group_by(Site)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  arrange(desc(Comp_Rate))

video_by_Site %>% formattable(align = c("l",rep("r", ncol(video_by_Site) - 1)), 
                              list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                                   `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                                   `Comp_Rate` = color_tile("#B1CBEB", "#3E7DCC")
                              )) 
```

Videos have the highest Completion Rates on Sites I, S, and C. Videos have the best CTRs on Sites R and C.        

The correlation between Impressions and 100% Viewed is very strong (r = 0.981), but Clicks and 100% Viewed have a very weak correlation (r = 0.0486). One explanation for this may be that some of these Video ads are unskippable and/or autoplay videos. Another explanation is that the videos may be very short and it isn't worth it for the viewer to click past a very short video ad. More clarifying information is needed to put the Completion Rate metric into context.  
   
**Recommendation:**   
**(R2)** Create a variable that clarifies the video ad format. Example formats: skippable in-stream, non-skippable in-stream, bumper ads, outstream ads, masthead ads.      
   
### (iii) Rich Media
```{r rich}
rich_by_Site <- rich_split%>%
  group_by(Site)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`), 
            Interactions = sum(Interactions))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  calc_Eng_Rate()%>%
  arrange(desc(Eng_Rate))

rich_by_Site %>% formattable(align = c("l",rep("r", ncol(rich_by_Site) - 1)), 
            list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Comp_Rate` = color_tile("#B1CBEB", "#3E7DCC"), 
                 `Eng_Rate` = color_tile("#F7E2DE", "#C98D6D")
                )) 
```

The metrics for Site A are completely different from the other Sites. Since Site A contains only four observations, I am going to remove them from the data set. 
    
Furthermore, there are two Creative Names for Rich Media ads: VideoRMB and AnimatedRMB. AnimatedRMB does not have any viewership data because there is no video on the ad. Therefore, the Completion Rate metric is not meaningful for AnimatedRMB Rich Media.    
       
The Completion Rates in the chart above are being skewed by the 0% Completion Rates of AnimatedRMB. To solve this problem, analysis will be divided between Creative Names.    
     
**Recommendation:**      
**(R3)** Segment analysis of Rich Media ads by Creative Names, VideoRMB and AnimatedRMB.     

   
``` {r sldk, message = FALSE}
rich_split<-rich_split%>%filter(Site != "Site A")

rich_by_Site_twoGroups <- rich_split %>%
    group_by(Site, `Creative Name`) %>%
    summarise(Impressions = sum(Impressions), 
              Clicks = sum(Clicks), 
              `100% viewed` = sum(`100% viewed`), 
              Interactions = sum(Interactions)) %>%
    calc_CTR()%>%
    calc_Comp_Rate()%>%
    calc_Eng_Rate()%>%
    arrange(Site)

```
     
**Creative Name: AnimatedRMB**   
```{r kdla}
rich_by_Site_twoGroups_Animated<-rich_by_Site_twoGroups%>%filter(`Creative Name` != "Product_KSP1_VideoRMB")%>%subset(select=-c(`Comp_Rate`, `100% viewed`, `Creative Name`))%>%arrange(desc(Eng_Rate))

rich_by_Site_twoGroups_Animated %>% formattable(align = c("l",rep("r", ncol(rich_by_Site_twoGroups) - 1)), 
            list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Eng_Rate` = color_tile("#F7E2DE", "#C98D6D")
                )) 
```

Site I has the highest Engagement Rate for AnimatedRMB Rich Media. Sites I and K have the highest CTR. There is a moderately strong correlation (r = 0.62) between CTR and Engagement Rate.    


**Creative Name: VideoRMB**
```{r dksla}

rich_by_Site_twoGroups_Video<-rich_by_Site_twoGroups%>%filter(`Creative Name` != "Product_KSP1_AnimatedRMB")%>%subset(select=-`Creative Name`)%>% arrange(desc(Eng_Rate))

rich_by_Site_twoGroups_Video %>% formattable(align = c("l",rep("r", ncol(rich_by_Site_twoGroups) - 1)), 
            list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Comp_Rate` = color_tile("#B1CBEB", "#3E7DCC"), 
                 `Eng_Rate` = color_tile("#F7E2DE", "#C98D6D")
                )) 
```


Site I has the highest Engagement Rate, Completion Rate, and CTR for VideoRMB Rich Media.  There is a moderately strong correlation (r = 0.76) between CTR and Engagement Rate. There is a moderate correlation (r = 0.46) between CTR and Completion Rate.     
      
VideoRMB and AnimatedRMB have similar Engagement Rates. The CTR for VideoRMB is slightly better.         
     
***    
     
## VI. Performance Evaluation - Device
### (i) Banner
```{r deviceB}
banner_by_Device <- banner_split%>%
  group_by(Device)%>%
  summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))%>%
  calc_CTR()%>%
  arrange(desc(CTR))

banner_by_Device %>% formattable(align = c("l",rep("r", ncol(banner_by_Device) - 1)), 
            list(`Device` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97")
                )) 
```

Mobile Device drives the highest CTR for Banner ads. 


### (ii) Video
```{r deviceV}
video_by_Device <- video_split%>%
  group_by(Device)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  arrange(desc(Comp_Rate))

video_by_Device %>% formattable(align = c("l",rep("r", ncol(video_by_Device) - 1)), 
            list(`Device` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Comp_Rate` = color_tile("#B1CBEB", "#3E7DCC")
                )) 
```

Mobile Device drives the highest CTR and Completion Rates for Video ads. 


### (iii) Rich Media

``` {r kkjkk, message = FALSE}
rich_by_Device_twoGroups <- rich_split %>%
    group_by(Device, `Creative Name`) %>%
    summarise(Impressions = sum(Impressions), 
              Clicks = sum(Clicks), 
              `100% viewed` = sum(`100% viewed`), 
              Interactions = sum(Interactions)) %>%
    calc_CTR()%>%
    calc_Comp_Rate()%>%
    calc_Eng_Rate()%>%
    arrange(desc(Eng_Rate))

rich_by_Device_twoGroups_Animated<-rich_by_Device_twoGroups%>%filter(`Creative Name` != "Product_KSP1_VideoRMB")%>%subset(select=-c(`Comp_Rate`, `100% viewed`, `Creative Name`))%>%arrange(desc(Eng_Rate))
rich_by_Device_twoGroups_Video<-rich_by_Device_twoGroups%>%filter(`Creative Name` != "Product_KSP1_AnimatedRMB")%>%subset(select=-`Creative Name`)%>% arrange(desc(Eng_Rate))
```

**Creative Name: AnimatedRMB**  
```{r kjkdla}
rich_by_Device_twoGroups_Animated %>% formattable(align = c("l",rep("r", ncol(rich_by_Device_twoGroups) - 1)), 
            list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Eng_Rate` = color_tile("#F7E2DE", "#C98D6D")
                )) 
```

Desktop Device drives the greatest Engagement Rate for AnimatedRMB Rich Media. Tablet Device drives the greatest CTR. 



**Creative Name: VideoRMB**
```{r dksjkla}
rich_by_Device_twoGroups_Video %>% formattable(align = c("l",rep("r", ncol(rich_by_Device_twoGroups) - 1)), 
            list(`Site` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Comp_Rate` = color_tile("#B1CBEB", "#3E7DCC"), 
                 `Eng_Rate` = color_tile("#F7E2DE", "#C98D6D")
                )) 
```


Desktop Device drives the greatest Engagement Rate for VideoRMB Rich Media. Tablet Device drives the greatest CTR and Completion Rate.     


***
## VII. Performance Evaluation - Creative Name      
### (i) Banner     
```{r creativeB}
banner_by_Creative <- banner_split%>%
  group_by(`Creative Name`)%>%
  summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))%>%
  calc_CTR()%>%
  arrange(desc(CTR))

banner_by_Creative %>% formattable(align = c("l",rep("r", ncol(banner_by_Creative) - 1)), 
            list(`Creative Name` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97")
                )) 
```

Banners have relatively similar CTR performance among Creative Names. "300off" Creatives perform slightly better than "200off". It does not appear that any "KSP" has an advantage over the other.    

### (ii) Video    
```{r creativeV}
video_by_Creative <- video_split%>%
  group_by(`Creative Name`)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  arrange(desc(Comp_Rate))

video_by_Creative %>% formattable(align = c("l",rep("r", ncol(video_by_Creative) - 1)), 
            list(`Creative Name` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Comp_Rate` = color_tile("#B1CBEB", "#3E7DCC")
                )) 
```

"200off" Video ads tend to have higher Completion Rates than "300off". However, "300off" Videos have slightly higher CTRs. It does not appear that any “KSP” has an advantage over the other.

### (iii) Rich Media   
```{r creativeRM}
rich_by_Creative <- rich_split%>%
  group_by(`Creative Name`)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`), 
            Interactions = sum(Interactions))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  calc_Eng_Rate()%>%
  arrange(desc(Eng_Rate))

rich_by_Creative %>% formattable(align = c("l",rep("r", ncol(rich_by_Creative) - 1)), 
            list(`Creative Name` = formatter("span", style = ~ style(font.weight = "bold")),
                 `CTR` = color_tile("#DeF7E9", "#71CA97"), 
                 `Comp_Rate` = color_tile("#B1CBEB", "#3E7DCC"), 
                 `Eng_Rate` = color_tile("#F7E2DE", "#C98D6D")
                )) 
```

*Note: Completion Rate is meaningful for the VideoRMB only.*
CTRs and Engagement Rates for both Creative Names are comparable.       

***

## VIII. Trends Over Time

### Combine Data
``` {r jksajk, message = FALSE, error = FALSE, warning = FALSE}
ALL<-full_join(banner_split, video_split)
ALL<-full_join(ALL, rich_split)
ALL_Video_Rich<-ALL%>%filter(`Creative Type` != "BAN" & `Creative Name` != "Product_KSP1_AnimatedRMB")
```



### (i) Clicks    
``` {r ksla, message=FALSE, warning = FALSE}
ALL_by_Date <- ALL%>%
  group_by(Date, `Creative Type`)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`), 
            Interactions = sum(Interactions))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  calc_Eng_Rate()%>%
  arrange(Date)

ALL_by_Date %>% ggplot(aes(
          x = Date, 
          y = Clicks, 
          group = `Creative Type`, 
          color = `Creative Type`)) + 
  geom_line() + 
  labs(title = "Total Clicks by Creative Type", 
          subtitle = "April 23 - July 30, 2018") + 
  ylab("Clicks") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "bold", hjust = 0.5)) + 
  scale_color_manual(name="Creative Type",
          values=c("red","green","blue"),
          labels=c("Banner", "Video", "Rich Media"))

```

All three Creative Types had spikes in Clicks on May 13 and June 14-16. Video has consistently more Clicks relative to Banner and Rich Media. Banners experience the greatest spiking effects.  

### (ii) CTR     
``` {r kjdsajkljakl}
ALL_by_Date %>% ggplot(aes(
          x = Date,
          y = CTR, 
          group = `Creative Type`, 
          color = `Creative Type`)) + 
  geom_line() +
  labs(title = "CTR by Creative Type", 
        subtitle = "April 23 - July 30, 2018") +
  ylab("CTR") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face = "bold", hjust = 0.5)) + 
  scale_color_manual(name="Creative Type",
      values=c("red","green","blue"),
      labels=c("Banner", "Video", "Rich Media"))
```
     
Video has consistently higher CTR. Video experienced spikes on June 25-27.    


### (iii) Completion Rate    
```{r trendsV, warning = FALSE, message = FALSE}

Video_Rich_by_Date <- ALL_Video_Rich%>%
  group_by(Date, `Creative Type`)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  arrange(Date)

Video_Rich_by_Date %>% ggplot(aes(
          x = Date, 
          y = Comp_Rate, 
          group=`Creative Type`, 
          color=`Creative Type`)) + 
  geom_line() + 
  labs(title = "Completion Rate by Video and Rich Media", 
       subtitle = "April 23 - July 30, 2018") + 
  ylab("Completion Rate") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face = "bold", hjust = 0.5)) + 
  scale_color_manual(name="Creative Type",
    values=c("green","blue"),
    labels=c("Video", "Rich Media"))
```

*Note: Rich Media excludes Animated Creative.*    
   
Video has consistently higher Completion Rates. Both Video & Rich Media Completion Rates appear to be on the decline.    

### (iv) Engagement Rate     
```{r trendsRM, warning = FALSE, message = FALSE}
rich_by_Date <- rich_split%>%
  group_by(`Date`)%>%
  summarise(Impressions = sum(Impressions), 
            Clicks = sum(Clicks), 
            `100% viewed` = sum(`100% viewed`), 
            Interactions = sum(Interactions))%>%
  calc_CTR()%>%
  calc_Comp_Rate()%>%
  calc_Eng_Rate()%>%
  arrange(Date)

rich_by_Date %>% ggplot(aes(
          x = Date, 
          y = Eng_Rate)) + 
  geom_line(color="blue") + 
  labs(title = "Engagement Rate on Rich Media", 
       subtitle = "April 23 - July 30, 2018") + 
  ylab("Engagement Rate") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face = "bold", hjust = 0.5))
```


*Note: Rich Media includes both VideoRMB & AnimatedRMB*   
   
Rich Media Engagement Rates experienced spikes on May 13, June 14-16.    

***
## IX. Recommendations for Improvement

**(R1)** To distinguish between accidental and intentional ad clicking, Bounce Rate and Exit Rate should be calculated.      
      
**(R2)** Create a variable that clarifies the video ad format for better analysis. Example formats: skippable in-stream, non-skippable in-stream, bumper ads, outstream ads, masthead ads.               
      
**(R3)** Segment analysis of Rich Media ads by Creative Names, VideoRMB and AnimatedRMB.     
      
**(R4)** Drop the Rich Media part of the ad campaign.         
- Rich Media has the lowest CTR performance across ad types.   
- Rich Media has lower Completion Rates compared to Video ads.    
- Rich Media cannot be viewed on Mobile Devices. Mobile is increasingly where consumers spend their time online, as shown by Mobile's performance on Banner and Video ads.        

**(R5)** If you are to keep Rich Media, focus resources on:      
  - VideoRMB instead of AnimatedRMB      
- Banner ads on Site I      
- Getting Rich Media ads onto Mobile Devices. Rich Media does pretty well on Tablets, but the Tablet market is much smaller than the Desktop and Mobile markets.      

**(R6)** Direct more resources towards top-performing Sites for Banner and Video. Banners do best on Sites J, C, M, N and I; Videos do best on Sites C, R, I, and S.        

**(R7)** Focus on Video and Mobile-friendly sites. Video is the best Creative Type for generating Clicks. Mobile Device drives the highest CTR and Completion Rates.          

**(R8)** Determine what caused spikes:    
  - in Clicks on May 13, June 14-16.   
  - in Video CTR on June 25-27.   
  - in Rich Media Engagement on May 13, June 14-16.         
  
**(R9)** Other KPIs to consider: Conversion Rate, Cost of Ad-Buy.   




* https://marketinginsidergroup.com/content-marketing/banners-99-problems/
  
