library(appler)
library(tidyverse)

# build the list of browers
browser = data.frame(name = c("safari", "edge", "chrome", "firefox", "opera"),
                     fullname = c("Safari", "Microsoft Edge", "Google Chrome",
                                     "Mozilla Firefox","Opera Browser"))

# build a function to acquire the app id from the Apple API
getid = function(name){
  id = search_apple(name, media = "software", entity = "software")[1,]
  trackId = id$trackId
  return(trackId)
}

# extract appid from the Apple API
browser$id = sapply(browser$fullname, getid)

# acquire the reviews of each browser
for (i in 1:dim(browser)[1]) {
  name = browser$name[i]
  review = get_apple_reviews(browser$id[i], country = "us", 
                    all_results = TRUE, sort_by = "mosthelpful")
  assign(name, review)
}
