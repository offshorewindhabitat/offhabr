import requests
from bs4 import BeautifulSoup
import csv

# URL for Tim Burton's filmography on iMDB
url = "https://www.imdb.com/name/nm0000318/filmotype"

# Send a request to the URL and store the response
response = requests.get(url)

# Parse the HTML of the response
soup = BeautifulSoup(response.text, 'html.parser')

# Find all the movie divs
movie_divs = soup.find_all('div', class_='filmo-row')
len(movie_divs)
# Create a CSV file to store the results
with open('tim_burton_movies.csv', 'w', newline='') as file:
  # Create a CSV writer
  writer = csv.writer(file)

  # Write the header row
  writer.writerow(['Title', 'Year', 'IMDB Rating'])

  # Loop through the movie divs
  for div in movie_divs:
    # Find the title and year of the movie
    title_year = div.find('h3', class_='lister-item-header').text.strip()
    title = title_year[:-7]
    year = title_year[-5:-1]

    # Find the IMDB rating
    rating = div.find('div', class_='ratings-imdb-rating').text.strip()

    # Write the movie's details to the CSV file
    writer.writerow([title, year, rating])

print("Scraping complete! Check the tim_burton_movies.csv file for the results.")
