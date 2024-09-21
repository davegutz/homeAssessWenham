"""
Read pdf files; extract text.
Next steps would be to parse the fields
from https://pymupdf.readthedocs.io/en/latest/the-basics.html
"""

"""
See this link for how to scrape asp content from assessors web site
    # https://www.bardeen.ai/answers/how-to-web-scrape-aspx-webpage
import requests
from bs4 import BeautifulSoup
import selenium
"""

import pymupdf
doc = pymupdf.open("wenhamranch1.pdf")
out = open("wenhamranch1.txt", "wb") # create a text output
for page in doc: # iterate the document pages
    text = page.get_text().encode("utf8") # get plain text (is in UTF-8)
    out.write(text) # write text of page
    out.write(bytes((12,))) # write page delimiter (form feed 0x0C)
out.close()
