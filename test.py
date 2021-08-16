from selenium.webdriver.chrome.options import Options
from selenium import webdriver

url = 'https://github.com/'

options = Options()
options.add_argument("--headless")
options.add_argument("window-size=1400,1500")

driver = webdriver.Chrome(options=options)

# Navigate to github.com
driver.get(url)

# Extract the top heading from github.com
text = driver.find_element_by_class_name('h000-mktg').text

print(text)
