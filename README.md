# Eurovision Song Contest &nbsp; :microphone:

The following project uses data from the [Eurovision Song Contest site](https://eurovision.tv/); which is pulled from the [Eurovision archives page](https://eurovision.tv/history). Data was scraped using R and packages including **rvest**. The resulting dataset represents information for all contestants for each year and by round (e.g. semi final, final). It is important to note that changes have occurred to the scoring system through different points in time, e.g. semi final rounds were not introduced until 2005. 

Inspiration for the project is from [Tanya Shapiro](https://github.com/tashapiro) and her [eurovision contest repo](https://github.com/tashapiro/eurovision-contest). Sadly, Eurovision changed its website structure hence the project to update this great resource. 

## About

Excerpt taken from [Wikipedia](https://en.wikipedia.org/wiki/Eurovision_Song_Contest):

>The Eurovision Song Contest (French: Concours Eurovision de la chanson), sometimes abbreviated to ESC and often known simply as Eurovision, is an international songwriting competition organised annually by the European Broadcasting Union (EBU), featuring participants representing primarily European countries. Each participating country submits an original song to be performed on live television and radio, transmitted to national broadcasters via the EBU's Eurovision and Euroradio networks, with competing countries then casting votes for the other countries' songs to determine a winner.

>Based on the Sanremo Music Festival held in Italy since 1951, Eurovision has been held annually since 1956 (apart from 2020), making it the longest-running annual international televised music competition and one of the world's longest-running television programmes. Active members of the EBU, as well as invited associate members, are eligible to compete, and as of 2022, 52 countries have participated at least once. Each participating broadcaster sends one original song of three minutes duration or less to be performed live by a singer or group of up to six people aged 16 or older. Each country awards two sets of 1–8, 10 and 12 points to their favourite songs, based on the views of an assembled group of music professionals and the country's viewing public, with the song receiving the most points declared the winner. Other performances feature alongside the competition, including a specially-commissioned opening and interval act and guest performances by musicians and other personalities, with past acts including Cirque du Soleil, Madonna and the first performance of Riverdance. Originally consisting of a single evening event, the contest has expanded as new countries joined (including countries outside of Europe, such as Australia), leading to the introduction of relegation procedures in the 1990s, and eventually the creation of semi-finals in the 2000s. As of 2022, Germany has competed more times than any other country, having participated in all but one edition, while Ireland holds the record for the most victories, with seven wins in total.

## Data Dictionary 

Data is available [here](https://github.com/andrewmoles2/eurovision-contest/blob/main/data/eurovision_data.csv). 

To load data, you can use the below code snippets. 

For R:

```{r}
# option 1 - load using readr
eurovision <- readr::read_csv("https://raw.githubusercontent.com/andrewmoles2/eurovision-contest/main/data/eurovision_data.csv")

# option 2 - load using base R read.csv
eurovision <- read.csv("https://raw.githubusercontent.com/andrewmoles2/eurovision-contest/main/data/eurovision_data.csv")
``` 

For Python:

```{python}
import pandas as pd

eurovision = pd.read_csv("https://github.com/andrewmoles2/eurovision-contest/blob/main/data/eurovision_data.csv")
```

Short summary of fields in the dataset:

| Field               | Description                                          |
|:--------------------|:-----------------------------------------------------|
| event               | Event Name, e.g. Helsinki 2007                       |
| host_city           | Host city name, e.g. Helsinki                        |
| host_country        | Host city country, e.g. Finland                      |
| year                | Event year, e.g. 2007                                |
| round               | Round in contest, e.g. Semi-Final or Final           |
| participant_name    | Name of performer/participant                        |
| song                | Song title name                                      |
| participant_country | Participant country, e.g. Austria                    |
| running_order       | Order participant sang in contest                    |
| total_points        | Points                                               |
| rank                | Ordinal rank, e.g. 2nd                               |
| qualified           | Applicable to semi-final rounds only (TRUE or FALSE) |
| winner              | Applicable to final & semi-final rounds (TRUE or FALSE)      |
| event_link          | Link to event                                        |

## Example visualisations made with the data

These visualisations were made with R and the `ggplot2` package. The code used to produce them can be found in the [code folder](https://github.com/andrewmoles2/eurovision-contest/blob/main/code/eurovision_viz.R) in the `eurovision_viz.R` file. 

![](visuals/eurovision_winners.png)

![](visuals/eurovision_yearly_ranking.png)

![](visuals/eurovision_maps.png)


