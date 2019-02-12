from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import csv
import re


driver = webdriver.Chrome()

#this stops chrome from opening web images
chromeOptions = webdriver.ChromeOptions()
prefs = {'profile.managed_default_content_settings.images':2}
chromeOptions.add_experimental_option("prefs", prefs)
driver = webdriver.Chrome(chrome_options=chromeOptions)

driver.get("https://medium.com/topic/design")
time.sleep(25)


csv_file = open('medium.csv', 'w', encoding='utf-8', newline='\n')
writer = csv.writer(csv_file)


SCROLL_PAUSE_TIME = 5

# Get scroll height
last_height = driver.execute_script("return document.body.scrollHeight")

counter=0
while counter<7:
    counter+=1
    # Scroll down to bottom
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")

    # Wait to load page
    time.sleep(SCROLL_PAUSE_TIME)

    # Calculate new scroll height and compare with last scroll height
    new_height = driver.execute_script("return document.body.scrollHeight")
    if new_height == last_height:
        break
    last_height = new_height

# List of Links
#links = list(map(lambda x: x.get_attribute("href"), driver.find_elements_by_xpath('.//h3[@class="ai y cl at cm au ed fp fq d an ef cr"]/a'))) #tech
#links = list(map(lambda x: x.get_attribute("href"), driver.find_elements_by_xpath('.//h3[@class="ai y cl at cm au ed fl fm d an ef cr"]/a'))) #self
#links = list(map(lambda x: x.get_attribute("href"), driver.find_elements_by_xpath('.//h3[@class="ai y cl at cm au ed fl fm d an ef cr"]/a'))) #health
#links = list(map(lambda x: x.get_attribute("href"), driver.find_elements_by_xpath('.//h3[@class="ai y cl at cm au ed fl fm d an ef cr"]/a'))) #politics
#links = list(map(lambda x: x.get_attribute("href"), driver.find_elements_by_xpath('.//h3[@class="ai y cl at cm au ed fl fm d an ef cr"]/a'))) #startups
links = list(map(lambda x: x.get_attribute("href"), driver.find_elements_by_xpath('.//h3[@class="ai y cl at cm au ed fp fq d an ef cr"]/a'))) ###design



print(links)
# Passing links into lst
lst = links
for i in range(len(lst)):
    print('we are in the for loop')
    i_dict = {}
    driver.get(lst[i])
    time.sleep(.1)
    if i == 0:
        time.sleep(.1)
        try:
            title = driver.find_element_by_xpath('.//div[@class="section-inner sectionLayout--insetColumn"]/h1').text
        except:
            title = ''
        try:
            author = driver.find_element_by_xpath('.//a[@class="ds-link ds-link--styleSubtle ui-captionStrong u-inlineBlock link link--darken link--darker"]').text
        except:
            author = '' 
        try:
            date = driver.find_element_by_xpath('.//div[@class="ui-caption u-noWrapWithEllipsis js-testPostMetaInlineSupplemental"]/time').get_attribute("datetime")
        except:
            date = ''
        try:
            length = driver.find_element_by_xpath('.//span[@class="readingTime"]').get_attribute("title")
        except:
            length = ''
        try:
            claps = driver.find_element_by_xpath('.//span[@class="u-relative u-background js-actionMultirecommendCount u-marginLeft16"]/button').text
        except:
            claps = 0
        try:
            tags = list(map(lambda x: x.text, driver.find_elements_by_xpath('.//a[@class="link u-baseColor--link"]')))
        except:
            tags = []
    else:
        try:
            title = driver.find_element_by_xpath('.//div[@class="section-inner sectionLayout--insetColumn"]/h1').text
        except:
            try:
                title = driver.find_element_by_xpath('.//div[@class="uiScale uiScale-ui--regular uiScale-caption--regular"]/h1').text
            except:
                try:
                    title = driver.find_element_by_xpath('.//h1[@class="elevate-h1 u-marginBottom12 u-md-marginBottom8"]').text
                except:
                    title = ''
        try:
            author = driver.find_element_by_xpath('.//a[@class="ds-link ds-link--styleSubtle ui-captionStrong u-inlineBlock link link--darken link--darker"]').text
        except:
            try:
                author = driver.find_element_by_xpath('.//a[@class="ds-link ds-link--styleSubtle postMetaInline postMetaInline--author"]').text
            except:
                author = '' 
        try:
            date = driver.find_element_by_xpath('.//div[@class="ui-caption u-noWrapWithEllipsis js-testPostMetaInlineSupplemental"]/time').get_attribute("datetime")
        except:
            try:
                date = driver.find_element_by_xpath('.//time[@class="u-inlineBlock u-lineHeightBase"]').text
            except:
                date = ''
        try:
            length = driver.find_element_by_xpath('.//span[@class="readingTime"]').get_attribute("title")
        except:
            length = ''
        try:
            claps = driver.find_element_by_xpath('.//span[@class="u-relative u-background js-actionMultirecommendCount u-marginLeft16"]/button').text
        except:
            claps = 0
        try:
            tags = list(map(lambda x: x.text, driver.find_elements_by_xpath('.//a[@class="link u-baseColor--link"]')))
        except:
            tags = []


        i_dict['title'] = title
        i_dict['author'] = author
        i_dict['date'] = date
        i_dict['length'] = length
        i_dict['claps'] = claps
        i_dict['tags'] = tags
        writer.writerow(i_dict.values())



#<span class="u-relative u-background js-actionMultirecommendCount u-marginLeft16"><button class="button button--chromeless u-baseColor--buttonNormal js-multirecommendCountButton u-textColorDarker" data-action="show-recommends" data-action-value="41ae2749fff8">1 clap</button><span class="u-xs-hide"><div class="postMetaInline">Applause from you</div></span></span>
#<button class="button button--chromeless u-baseColor--buttonNormal js-multirecommendCountButton" data-action="show-recommends" data-action-value="41ae2749fff8">1 clap</button>

#response.xpath('.//button[@class="button button--chromeless u-baseColor--buttonNormal js-multirecommendCountButton"]')
    print(title)
    print(author)
    print(date)
    print(length)
    print(claps)
    print(tags)


#//*[@id="_obv.shell._surface_1549569044233"]/div/main/article/div/section/div[2]/div/div/div[2]/div[2]/time
#<time datetime="2019-02-07T13:58:48.983Z">Feb 7</time>

csv_file.close()
driver.close()

#way to handle the tags
#I expand the tag list to multiple columns
#then you meld them into one