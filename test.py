from selenium import webdriver
from selenium.webdriver.chrome.options import Options
options = Options()
options.headless = True
driver = webdriver.Chrome(options=options)
driver.get("https://www.youtube.com/")
element_text = driver.find_element_by_id("title").text
print(element_text)
