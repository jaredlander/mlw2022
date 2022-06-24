# Reading data ####

library(readr)
read_csv
read.csv

housing <- read_csv('https://jaredlander.com/data/housing.csv')
housing
class(housing)

library(dplyr)

# grammar of data manipulation

housing

# Select columns ####


select(housing, Neighborhood, Total.Units, Year.Built)
print(select(housing, Neighborhood, Total.Units, Year.Built), n=35)

print(housing, n=35)

head(housing)
head(housing, n=8)
tail(housing)
tail(head(housing, n=4), n=1)

head(housing)
housing |> head()
housing |> head(n=4)

housing |> head(n=4) |> tail(n=1)
tail(head(housing, n=4), n=1)

# %>%
# |>

select(housing, Neighborhood, Total.Units, Year.Built)
housing |> select(Neighborhood, Total.Units, Year.Built)


housing |>
    select(Neighborhood, Total.Units, Year.Built) |>
    head(n=4) |>
    tail(n=1)


# Choosing rows ####

housing |> slice(4)
housing |> slice(4, 8)

housing |> select(Hood=Neighborhood, Units=Total.Units)
housing

housing |> filter(Total.Units <= 100)
housing |> filter(Total.Units <= 100 & Year.Built >= 1985)

# & &&
# | ||

# if(a == 7 && b == 6)
# if(a == 7 & b == 6)

housing |> filter(Total.Units > 100 | Boro == 'Brooklyn')


housing |>
    filter(Boro == 'Brooklyn' | Boro == 'Manhattan' | Boro == 'Queens')

housing |>
    filter(Boro %in% c('Brooklyn', 'Manhattan', "Queens"))


# Counting rows ####

housing |> count(Boro)
housing |> count(Boro, Neighborhood)
housing |> count(Boro, Neighborhood) |> arrange(n)
housing |> count(Boro, Neighborhood) |> arrange(-n)
"R" > "Python"

housing |> count(Boro, sort=TRUE)

housing |> count(Boro) |> arrange(desc(Boro))

# Changing data.frames (kinda) ####

housing
house <- housing |>
    select(
        Neighborhood, Boro, Units=Total.Units, Built=Year.Built,
        Value=Full.Market.Value, SqFt=Gross.SqFt
    )
housing
house

house |> mutate(Value / SqFt)
house
house |> mutate(ValSqFt= Value / SqFt)
house |> mutate(ValSqFt=Value / SqFt, EvenBetter=ValSqFt*2)


# Summaries ####

# SELECT avg(Value) AS Value, Boro FROM house GROUP BY Boro

house |> filter(Boro == 'Manhattan') |> summarize(Value=mean(Value))
house |> filter(Boro == 'Brooklyn') |> summarize(Value=mean(Value))


house |>
    group_by(Boro) |>
    summarize(AvgValue=mean(Value))



house |>
    group_by(Boro) |>
    summarize(AvgValue=mean(Value), TotalValue=sum(Value))

house |>
    group_by(Boro, Neighborhood) |>
    summarize(AvgValue=mean(Value), TotalValue=sum(Value))

house |>
    filter(Units < 100) |>
    group_by(Boro, Neighborhood) |>
    summarize(AvgValue=mean(Value), TotalValue=sum(Value)) |>
    arrange(-AvgValue)


# Plotting ####
library(ggplot2)


# Value vs SqFt, for each Boro,
# with a smoothing curve (trend line, or best fit)

ggplot(house, aes(x=SqFt, y=Value, color=Boro)) +
    geom_point(shape=1, size=1, show.legend=FALSE) +
    geom_smooth(show.legend=FALSE) +
    scale_y_continuous(labels=scales::dollar) +
    facet_wrap(~Boro)


library(plotly)
ggplotly(
    ggplot(house, aes(x=SqFt, y=Value, color=Boro)) +
        geom_point(shape=1, size=1, show.legend=FALSE) +
        geom_smooth(show.legend=FALSE) +
        scale_y_continuous(labels=scales::dollar) +
        facet_wrap(~Boro)
)


places <- jsonlite::fromJSON(
    'https://jaredlander.com/data/FavoriteSpots.json'
)
