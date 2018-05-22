# zoopa_scrape
# Setup
setwd("G:/GRPRESCH/Louis/R/R-projects")
require(XML)
require(stringr)
require(rvest)
require(dplyr)

# source: https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/
rm(list = ls())

# postcode needed
# postcodes = read.csv("data/more_data_needed_pc.csv", stringsAsFactors = FALSE, header = TRUE)

# save psotcodes as values
pc = c("DA2",	"EC1Y",	"EC2A",	"EC4V",	"EN9",	"KT10",	"N1C",	"SW1Y",	"W1K",	"WC1B",	
       "WC1R",	"WC1X",	"WC2B",	"WC2N",	"WD23",	"EC2V",	"KT18",	"RM13",	"WC1V",	"EC1R",	
       "EC2Y",	"KT8",	"W1G",	"W1U",	"WC1H",	"WC1N",	"WC2H",	"WC2R",	"RM5", "EC1V",	
       "SE11",	"SE17",	"SW1P",	"E1W", "RM2", "SE27",	"SM7",	"W1T",	"N19",	"SE8",	
       "SW1X",	"SE24",	"DA6",	"IG7",	"KT7", "TW13",	"TW4",	"SE14",	"IG5", "SE20",	
       "SE21",	"SE7",	"SW9",	"UB1",	"BR4",	"IG2",	"IG4",	"RM14",	"RM7", "SW14",	
       "SW1W",	"W1H",	"DA17",	"SE28",	"SE12",	"SW1V",	"UB2", "SE19", "SE4",	"SM2",	
       "SW5",	"N16",	"NW5",	"TW3",	"DA5",	"IG6",	"KT9",	"N8",	"SE5",	"SW8",	
       "SE10",	"TW5",	"N18",	"N5",	"N7",	"TW14",	"UB6",	"HA6",	"NW4",	"N13",	
       "SW7",	"N15",	"RM8",	"SW12",	"UB5",	"DA14",	"N10", "SE16",	"SE26",	
       "EN4",	"KT5",	"N14",	"RM6",	"SE13",	"SE18",	"SE22",	"SM5",	"UB7",
       "BR2",	"IG1",	"SE3",	"SW10",	"TW8",	"UB3",	"DA15",	"IG3",	"RM10",
       "CR8",	"EN3",	"RM9",	"SE23",	"UB4")

# scrape attr. + description 
for(i in pc) {
  
  site <- paste("https://www.zoopla.co.uk/for-sale/houses/",i,"/?page_size=100&radius=0&results_sort=newest_listings&search_source=refine", sep="")
  zoopla <- read_html(site)
                
  z2 = zoopla %>%
    html_nodes(".listing-results-attr") %>%
    html_text()
  
  output1 <- matrix(unlist(z2), ncol = 3, byrow = TRUE) %>%
    as.data.frame() %>%
    select(-V3)
  
  newname <- paste("output1_", i, sep="")
  
  assign(newname,output1)
  rm(output1, zoopla)
  
}

# scrape price + address
for(i in pc) {
  
  site <- paste("https://www.zoopla.co.uk/for-sale/houses/",i,"/?page_size=100&radius=0&results_sort=newest_listings&search_source=refine", sep="")
  zoopla <- read_html(site)
  
  z1 = zoopla %>%
    html_nodes(".listing-results-address , .text-price") %>%
    html_text()
  
  output1 <- matrix(unlist(z1), ncol = 2, byrow = TRUE) %>%
    as.data.frame()
  
  newname <- paste("output2_", i, sep="")
  
  assign(newname,output1)
  rm(output1, zoopla)
  
}

# join
for(i in pc) {
  
  p1 = lapply(intersect(ls(pattern = i), ls(pattern = "output1")), function(x) get(x))
  p2 = lapply(intersect(ls(pattern = i), ls(pattern = "output2")), function(x) get(x))
  
  p1df = as.data.frame(p1)
  p2df = as.data.frame(p2)
  
  a = nrow(p2df) - nrow(p1df) + 1
  b = nrow(p2df) + 0
  
  p2df = p2df[a:b,]
  
  newname <- paste("dat_", i, sep="")
  
  output = cbind(p1df,p2df)
  assign(newname,output)
  
  rm(p1df, p2df, p1, p2, a, b, output)

}

# list = lapply(ls(pattern = "dat"), function(x) get(x))


# Merge dfs
dat = paste("dat_",pc,sep="")

dat_list = lapply(dat, function(i) {
  df = get(i)
  return(df)
})

dat_all = do.call(rbind, dat_list)
write.csv(dat_all, "data/extra_pcs.csv")

####
####   rm(list=ls(pattern="dat"))
####


### MANUAL SCRAPE  ###
  
zoopla <- read_html("https://www.zoopla.co.uk/for-sale/houses/DA5/?page_size=100&radius=0&results_sort=newest_listings&search_source=refine")

# beds, bathrooms, receptions
z2 = zoopla %>%
  html_nodes(".listing-results-attr") %>%
  html_text()

output2 <- matrix(unlist(z2), ncol = 3, byrow = TRUE) %>%
  as.data.frame() %>%
  select(-V3)

# price, address
z1 = zoopla %>%
  html_nodes(".listing-results-address , .text-price") %>%
  html_text()

output1 <- matrix(unlist(z1), ncol = 2, byrow = TRUE) %>%
  as.data.frame()

a = nrow(output1) - nrow(output2) + 1
b = nrow(output1) + 0

output1 = output1[a:b,]

# bind
output_DA2 = cbind(output1,output2)
rm(output1, output2)


########################################
# save  data
mixed_dat_3bed = rbind(output_1, output_2, output_3, output_4, output_5, output_6, 
                  output_7, output_8, output_9, output_10, output_11, output_12, 
                  output_13, output_14, output_15, output_16, output_17, output_18,
                  output_19, output_20)
# write.csv(mixed_dat_3bed, "data/mixed_3bed_data.csv")


