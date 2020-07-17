library("tidyverse")
library("ggthemes")
library("gganimate")
library("readxl")
library("httr")

GET("https://www.dshs.texas.gov/coronavirus/TexasCOVID-19ActiveCaseDatabyCounty.xlsx", write_disk(TXDSHS_file <- tempfile(fileext = ".xlsx")))
wide <- read_xlsx(TXDSHS_file, sheet = NULL, skip = 2, col_names = TRUE, n_max = 254)
names(wide) = janitor::make_clean_names(names(wide))

long <- wide %>%
	gather(Date, Cases, -x1, -county) %>%
	rename(County = county) %>%
	mutate(Date = as.Date(str_remove(Date, "active_cases_"), format = "%m_%d")) %>%
	mutate(Cases = as.integer(Cases)) %>%
	mutate(County = reorder(County, desc(Cases), sum))

top5 <- c("Harris", "Dallas", "Tarrant", "Bexar", "Travis")

line <- long %>% filter(County %in% top5) %>%
	ggplot(aes(Date, Cases, color = County, label = Cases)) + 
	geom_line(alpha = 0.7, show.legend = TRUE) + 
	geom_point(alpha = 0.1, show.legend = TRUE) +
	geom_vline(xintercept = as.numeric(as.Date("2020-05-01")), color = "gray") + 
	annotate("text", x = as.Date("2020-05-01"), y = 6500, label = "Texas Re-opens » ", color = "gray", hjust = 1) +
	geom_vline(xintercept = as.numeric(as.Date("2020-07-03")), color = "gray") +
	annotate("text", x = as.Date("2020-07-03"), y = 25000, label = "Abbott Issues Statewide Mask Mandate » ", color = "gray", hjust = 1) +
	labs(title="Estimated Active Coronavirus Cases in Texas' 5 Most Populous Counties", 
		subtitle="April 7, 2020 — July 8, 2020", 
		caption=paste("Source: Texas Department of State Health Services, ", Sys.Date(), sep=""), 
		x="", 
		y="") + 
	theme_solarized_2(light = FALSE)
		
line