library(dplyr)
library(rvest)
library(stringr)

t_url = 'https://www.walkscore.com/PA/Pittsburgh'
t_page = read_html(t_url)
t_df = data.frame(t_page %>% html_table())
colnames(t_df) = c('rank','neighborhood','walk_score','transit_score','bike_score','population')
t_df = t_df %>% filter(rank<=16)
t_df$neighborhood = tolower(gsub(' ', '-', t_df$neighborhood))
t_df$neighborhood[4] = 'south-side-flats'
head(t_df)

n_url1 = 'https://www.niche.com/places-to-live/search/best-places-for-young-professionals/m/pittsburgh-metro-area/?type=cityNeighborhood&type=city&whoLivesHere=youngProfessionals&walkability=careALot'
n_url2 = 'https://www.niche.com/places-to-live/search/best-places-for-young-professionals/m/pittsburgh-metro-area/?whoLivesHere=youngProfessionals&walkability=careALot&type=cityNeighborhood&type=city&page=2'
n_page1 = read_html(n_url1)
n_page2 = read_html(n_url2)

neighborhoods1 = tolower(n_page1 %>% html_nodes('h2') %>% html_text())
neighborhoods2 = tolower(n_page2 %>% html_nodes('h2') %>% html_text())
neighborhoods = c(neighborhoods1, neighborhoods2)
neighborhoods = gsub(' ', '-', neighborhoods)
neighborhoods_w_t = neighborhoods[neighborhoods %in% tolower(gsub(' ', '-', t_df$neighborhood))]
neighborhood_hrefs = paste0('https://www.niche.com/places-to-live/n/', neighborhoods_w_t, '-pittsburgh-pa/')

neighborhood_df = data.frame()
for(i in 1:length(neighborhood_hrefs)) {
  temp_url = neighborhood_hrefs[i]
  temp_neighborhood = neighborhoods_w_t[i]
  temp_page = read_html(temp_url)
  
  temp_cats = temp_page %>% html_nodes('ol') %>% html_nodes('li') %>% html_nodes('div') %>% html_nodes('.profile-grade__label') %>% html_text()
  temp_scores = temp_page %>% html_nodes('ol') %>% html_nodes('li') %>% html_nodes('div') %>% html_nodes('.niche__grade') %>% html_text()
  temp_labels = temp_page %>% html_nodes('.scalar__label') %>% html_nodes('span') %>% html_text()
  temp_values = temp_page %>% html_nodes('.scalar__value') %>% html_nodes('span') %>% html_text()
  
  temp_score_df = data.frame('neighborhood'=temp_neighborhood, 'public_schools'=temp_scores[1], 
                             'crime_and_safety'=temp_scores[2], 'housing'=temp_scores[3], 
                             'nightlife'=temp_scores[4], 'good_for_families'=temp_scores[5],
                             'diversity'=temp_scores[6], 'jobs'=temp_scores[7], 'weather'=temp_scores[8],
                             'cost_of_living'=temp_scores[9], 'health_and_fitness'=temp_scores[10],
                             'outdoor_activities'=temp_scores[11], 'commute'=temp_scores[12],
                             'median_home_val'=temp_values[2], 'median_rent'=temp_values[3],
                             'median_household_income'=temp_values[5], 
                             'walk_score'=t_df[t_df$neighborhood==temp_neighborhood,'walk_score'],
                             'transit_score'=t_df[t_df$neighborhood==temp_neighborhood,'transit_score'],
                             'bike_score'=t_df[t_df$neighborhood==temp_neighborhood,'bike_score'])
  
  if(nrow(neighborhood_df)==0) {
    neighborhood_df = temp_score_df
  } else {
    neighborhood_df = rbind(neighborhood_df, temp_score_df)
  }
}
neighborhood_df

# get reviews
reviews_hrefs = paste0(neighborhood_hrefs, 'reviews/')

temp_url = reviews_hrefs[7]
temp_page = read_html(temp_url)


temp_page %>% html_nodes('.reviews-expansion-bucket') %>% html_nodes('.review') %>% 
  html_nodes('.review__text') %>% html_text()

# distance from the city
