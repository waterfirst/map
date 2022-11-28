# map


library(maps)
data(world.cities)

head(world.cities) # 데이터가 어떻게 생성되어있고 어떤변수들 무엇을 뜻한지 알아봐야한다

skorea.pop <- world.cities[world.cities$country.etc %in% "Korea South",]

skorea.pop <- skorea.pop[order(skorea.pop$pop),]

skorea.pop <- tail(skorea.pop,10)

skorea.pop <- skorea.pop[-10,]

# South Korea에 해당하는 모든 도시들을 skorea.pop에 따로 저장해놓고 서울을 제외한 인구수가 가장많은 9개의 도시들을 골라냈다

nkorea.pop <- world.cities[world.cities$country.etc %in% "Korea North",]

nkorea.pop <- nkorea.pop[order(nkorea.pop$pop),]

nkorea.pop <- tail(nkorea.pop,9)

korea.pop <- rbind(skorea.pop,nkorea.pop)


ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group, fill=region),colour="black") + scale_fill_brewer(palette="Set1")+ geom_point(data=korea.pop, aes(x=long, y=lat, size = pop), shape = 16, color = "green", alpha = 0.4)

ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group, fill=region),colour="black") + scale_fill_brewer(palette="Set1")+ geom_point(data=korea.pop, aes(x=long, y=lat, size = pop), shape = 16, color = "green", alpha = 0.4)+ scale_size_area(max_size=30)


ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group, fill=region),colour="black") + scale_fill_brewer(palette="Set1")+ geom_point(data=korea.pop, aes(x=long, y=lat, size = pop), shape = 16, color = "green", alpha = 0.4)+ scale_size_area(max_size=20)+ geom_text(data=korea.pop, aes(x=long+0.2, y=lat+0.2, label=name))

library(nycflights13)
glimpse(flights)

flights |>  
  filter(!is.na(arr_delay), !is.na(tailnum)) |> 
  group_by(tailnum) |> 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

mtcars |> pull(cyl)
mtcars %>% pull(cyl)

billboard

billboard_tidy <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )
billboard_tidy

billboard_tidy |> 
  ggplot(aes(week, rank, group = track)) + 
  geom_line(alpha = 1/3) + 
  scale_y_reverse()


## These all return 1000
parse_number("$1,000") ## leading `$` and grouping character `,` ignored
parse_number("euro1,000") ## leading non-numeric euro ignored
parse_number("t1000t1000") ## only parses first number found

parse_number("1,234.56")
## explicit locale specifying European grouping and decimal marks
parse_number("1.234,56", locale = locale(decimal_mark = ",", grouping_mark = "."))
## SI/ISO 31-0 standard spaces for number grouping
parse_number("1 234.56", locale = locale(decimal_mark = ".", grouping_mark = " "))

## Specifying strings for NAs
parse_number(c("1", "2", "3", "NA"))
parse_number(c("1", "2", "3", "NA", "Nothing"), na = c("NA", "Nothing")) 

library(gt)

start_date <- "2010-06-07"
end_date <- "2010-06-14"

sp500 %>%
  dplyr::filter(date >= start_date & date <= end_date) %>%
  dplyr::select(-adj_close) %>%
  gt() %>%
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) %>%
  fmt_date(
    columns = vars(date),
    date_style = 3
  ) %>%
  fmt_currency(
    columns = vars(open, high, low, close),
    currency = "USD"
  ) %>%
  fmt_number(
    columns = vars(volume),
    suffixing = TRUE
  )

# load data ---------------------------------------------------------------

df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )


library(DT)
datatable(iris)
