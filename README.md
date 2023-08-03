## GET /articles


### Get news articles from Gnews

### GET Parameters:

- keyword
     - **Description**: This parameter is a mandatory path variable. This parameter allows you to specify your search keywords to find the news articles you are looking for. The keywords will be used to return the most relevant articles. 

- articlesCount
     - **Description**: This query parameter allows you to specify the number of news articles returned by the API. The minimum value of this parameter is 1 and the maximum value is 100. Default value is 10.
     If we give a count > 100 or <= 0 , will return error response "Count should be greater than zero and less than or equal to 100".

- attribute
     - **Description**: This query parameter allows you to choose in which attributes the keywords are searched. The attributes that can be set are title, description and content. It is possible to combine several attributes by separating them with a comma.
     e.g. title,description


### Response:

- Example (`application/json`):

```javascript
{
  "articles": [
    {
      "content": "An dieser Stelle finden Sie einen relevanten Inhalt der externen",
      "description": "Derzeit sind auf den Nägeln haufenweise Blumen zu sehen.",
      "image": "https://ais-akamai.rtl.de/masters/2019139/1686x0/nageldesign.jpg",
      "publishedAt": "2023-07-24T12:25:56Z",
      "source": {
        "name": "RTL Online",
        "url": "https://www.rtl.de"
      },
      "title": "Flower-Nails: Der sommerliche Nageltrend",
      "url": "https://www.rtl.de/cms/flower-nails-der-sommerliche-nageltrend-5051912.html"
    },
    {
      "content": "Get ready for 45 acres of lavender, sunflowers, snap dragons",
      "description": "The Abbotsford Summer Flower Festival is happening for the first time in 2023 ",
      "image": "https://www.vmcdn.ca/f/files/via/images/events/abbotsford-summer-flower-festival.png;w=1200;h=800;mode=crop",
      "publishedAt": "2023-07-22T22:06:26Z",
      "source": {
        "name": "Vancouver Is Awesome",
        "url": "https://www.vancouverisawesome.com"
      },
      "title": "Lower Mainland's largest flower festival makes summer debut",
      "url": "https://www.vancouverisawesome.com/events-and-entertainment/largest-abbotsford-metro-vancouver-7312554"
    }
  ],
  "totalArticles": 4202
}
```

## How to call this API

### port number is predefined to 8001

## url

`http://localhost:8001/articles/<keyword>?articlesCount=<count>&attribute=<attribute>`


#### keyword -> string
#### count -> integer greater than 0 and less than or equal to 100
#### attribute -> string (eg:"title","description","title,description")


## How to run this haskell project

1. Clone the repository - `git clone https://github.com/Althaf12345/project-api.git`
2. Install redis 
3. Run redis-server in a seperate terminal
4. Change directory to project-api - `cd project-api`
5. Enter command - `stack ghci`
6. Load the file Main.hs in app folder of project-api - `:l app/Main.hs`
7. Call the function startApp which will start the application on port 8001 - `startApp`
8. Hit the endpoint - 
  `http://localhost:8001/articles/<keyword>?articlesCount=<count>&attribute=<attribute>`
