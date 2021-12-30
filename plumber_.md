#### API's & `plumber` [^1]


**API's 101**

Two different computers (client & server) interact with each other to request & provide data and allow data scientists
to request clean & curated data from a website. Contrasting the API approach with pure web scraping, the former enables
analysts to directly conduct analysis on the retrieved data without having to build lots of pre-processing functionality.


**How to make API requests in R?**

Two useful libraries: 

  `httr`: provides a nice framework for working with HTTP requests in R \
  `jsonlite`: most responses provided in JSON format, this package provides a wrapper to handle the data
  
Several types of request can be made to an API server. What's a request? An HTTP request can be thought of simply as a packet of information sent to the server, which the server attempts to interpret and respond to. The request types are:

   1. `GET`
   2. `POST`
   3. `DELETE`
   4. `PUT`

<ins>_Generic Example:_</ins> [Open Notify API](http://open-notify.org/Open-Notify-API/People-In-Space/) (Information on NASA projects).

```
res =  httr::GET(url = "https://api.open-notify.org/astros.json") # additional parameters can be passed here
res_ = jsonlite::fromJSON(rawToChar(base::rawToChar(res$content))) # raw Unicode > character vector > list
```

The output of `res` is in this case a list containing all the information returned by the API server. It also provides an attribute `Content-Type: application/json` and `Status: 200` which tells us whether the request was successful or not (200 refers to success [^2]).


**How to create API endpoints in R?**

Traditionally,  moving models (or R code) into production required either translating the work done in R into another 
language or running batch processes and storing the data in a database. The former is costly and the latter works but model output is served with a lag (running interactive simulations is not possible using this approach). The _optimal 
solution is to expose the R code as a service_. Furthermore, this software as a service framework allows data scientists to do their work in any programming language of which the downstream consumers of these APIs require no knowledge of. The [`plumber`](https://rviews.rstudio.com/2018/07/23/rest-apis-and-plumber/) package provides a framework to create REST API's in R. In Python, we can use [`FastAPI`](https://fastapi.tiangolo.com/).

<ins>_Example:_</ins> `plumber.R`

```
library(plumber)

#* @apiTitle Simple API

#* Echo provided text
#* @param text The text to be echoed in the response
#* @get /echo # this endpoint is available at api-url/echo & will respond to GET requests
function(text = "") {
  list(
    message_echo = paste("The text is:", text)
  )
}
```

To host the API, start a local web server hosting the API by running: `plumber::plumb("plumber.R")$run(port = 8001)`. Now that the API is running, we can query it in the command prompt:

```
$ curl "localhost:8001/echo?text=Hi%20there" | jq '.'
{
  "message_echo": [
    "The text is: Hi there"
  ]
}

```

To deploy API's, use [`RStudio Connect`](https://www.rstudio.com/products/connect/).


[^1]: `plumber` is an R package that allows existing R code to be exposed as a web service (API) through special decorator comments.
[^2]: see [here](https://www.restapitutorial.com/httpstatuscodes.html) for status codes

