Creating Github-based Interactive Graphics
================

Interactivity
=============

This weekend I saw a fantastic post shared by [datatitian](https://datatitian.com/how-to-turn-your-ggplot2-visualization-into-an-interactive-tweet/) on how to turn your interactive plots into a card that can be played with in a tweet. Not only is it supremely cool, but I can think of a whole new avenue for sharing visualizations if this strategy can be implemented easily by others.

In the blog post, Will Murphey uses his website to serve the visualization, and some other tools to help with the html tagging. Unfortunately, I do not currently have a website for serving my visualizations so I thought I would find a hack around that.

Github and its ecosystem of tools are my solution to this problem. I don't claim to say that I knew what I was doing. In fact, it was a lot of trial and error combined with Will's great tutorial.

Load Libraries
==============

Following the pattern from the blog post, I needed the `htmlwidgets` library. For data processing and collection, I used the `tidyverse` and `rvest` libraries. For generating an interactive plot, I will be using the fantastic `leaflet` library.

``` r
library(tidyverse)
library(leaflet)
library(rvest)
library(htmlwidgets)
```

Dataset
=======

The dataset I chose to use is scraped from wikipedia, and is a list of cities in the US and their populations. This dataset also included the latitude and longitudes of the cities, helping make it a 'one-spot-shop' for me to get everything for my visualization.

``` r
population_table<-read_html("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population")%>%
  html_table()%>%
  `[[`(5)

population_table_cleaned<-population_table%>%
  set_names(c("2017rank","City","State","2017estimate","2010Census" ,"Change",
              "2016 land area(mi2)","2016 land area(km2)",
              "2016 population density(mi2)","2016 population density(ki2)",
              "Location" ))%>%
  select(City,cencus_population=`2010Census`,Location)%>%
  mutate(Location = gsub(".+ ([-]*(\\d+)[.](\\d+))[;] ([-]*(\\d+)[.](\\d+)).*","\\1;\\4",Location),
         City=gsub("\\[.*\\]","",City)) %>%
  separate(Location,c("lat","long"),";")%>%
  mutate(lat=as.numeric(lat),
         long=as.numeric(long),
         label=paste0(City,"\nPopulation:",cencus_population),
         population=as.numeric(gsub(",","",cencus_population)))
```

Plotting
========

Making the viz was based on reading through the examples and some trial and error. I created some helper functions to provide scaling of the datasets to be fed into the leaflet object.

``` r
calcColor<-function(x,colors=c("white","blue"),...,granularity=100){
  colfunc <- colorRampPalette(colors,...)
  colors<-colfunc(granularity)
  newx<-round((granularity-1 ) * ((x - min(x)) / (max(x) - min(x))))+1
  colors[newx]
}

calcRadius<-function(x,maxsize=100,minsize=5,method=scale_sigmoid){
    oneScaled<-((x - min(x)) / (max(x) - min(x)))
    (maxsize - minsize) * method(oneScaled)  + minsize;
}

scale_sigmoid<-function(x){
  (tanh((x-.5)*2*pi)+1)/2
}

scale_linear<-function(x){
  x
}


ll<-leaflet(population_table_cleaned) %>% 
  addTiles() %>%
  addCircleMarkers(
    lng= ~long,
    lat= ~lat,
    radius = ~calcRadius(population,maxsize = 20,minsize=5),
    popup = ~label,
    color = ~calcColor(population,color=c("blue","red")),
    stroke = FALSE,
    fillOpacity = 0.5
  )
```

Saving
======

In order to make the plot useable, I need to save the plot using the "saveWidget" function from `htmlwidgets`

``` r
saveWidget(ll,
           "population_widget.html",     
           libdir = "lib",
           selfcontained = FALSE,
           title = "Population Density")
```

Making the Twitter card
=======================

Now the most important part of the process is editing the html to add the necessary meta elements to make twitter recognize how to handle the visualization and render it inside the twitter card.

Will Murphey had provided a list of the tags he used, but not how to add them. Since he was using wordpress to host his sites, he was able to use a tool to add it. Since, I am not I went into the html that is generating the blog post and figured out the meta tags he used and their formats. I then altered them for my use.

Rendering HTML
--------------

The most important meta element appears to be the 'twitter:player' meta element. This is where you tell twitter the URL to go to to render in the card. Since github's raw file viewer prevents the rendering of the HTML directly this causes some issues. Thankfully I found a site, called [https://raw.githack.com/'](https://raw.githack.com/) that converts the raw urls into fully served webpages. it has two options for production or testing. Since we want to be nice to the developer, I will be using the production version. This uses CloudFlare's CDN. All that needs to be done is to enter the url of the github file to render, and it generates a link to use.

Since I posted my visualization onto '<https://github.com/thebioengineer/Twidget/blob/master/population_widget.html>', I entered that into the websites converter. I was fed back the following url to use for production; '<https://rawcdn.githack.com/thebioengineer/Twidget/da435b815d4a845e651d932d4a8ffef71439a15b/population_widget.html>'. The following are the html

Adding the html elements
------------------------

Now that I had all the information I needed to enter into the meta elements, I opened the html file in RStudio and added them into the 'head' portion of the html file. I based the location based on the other 'meta' element.

![As you can see, there are a few options](editing_html.png)

Below is the text for you to copy:

    <!--Twitter interactive information-->
    <meta name="twitter:card" content="player"/>
    <meta name="twitter:site" content="@USERNAME"/> <!--twitter handle-->
    <meta name="twitter:title" content="Twidget: US Population"/> <!--twitter card title-->
    <meta name="twitter:description" content="Click to play! Twidget Serving through github"/> <!--twitter card description-->
    <meta name="twitter:player" content="GITHACK_CDN_URL" /> <!--raw.githack.com CDN url-->
    <meta name="twitter:player:width" content="517" /> 
    <meta name="twitter:player:height" content="408" />
    <meta name="twitter:image" content="http://www.edu.uwo.ca/img/click_to_play.png" /> <!--twitter card image before rendering player-->

Now that everything is there, I pushed to github.

The final Presentation!
=======================

Now that everything is set up, I can just put the raw link to the github code via '<https://github.com/thebioengineer/Twidget/blob/master/population_widget.html?raw=true>' into my twitter posts and it will render.

Once again, as Will Murphey said in his article, you can test your code and the visualization using the [twitter card validator](https://cards-dev.twitter.com/validator). Thank you so much for not only sharing this AWESOME idea, but the steps you took to perform this. It was inspiring and very exciting for me to hack this together on my sunday afternoon.

![The Results!](testing_html.png)
