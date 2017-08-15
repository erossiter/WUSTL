from bs4 import BeautifulSoup
import urllib2 

###############
## Problem 1 ##
###############

## Reading in html 
web_page = urllib2.urlopen("file:///Users/erinrossiter/Dropbox/Github/WUSTL/Debate1.html")
soup = BeautifulSoup(web_page.read())

## "p" tags hold statements
statements = soup.find_all("p")

participants = ["LEHRER:", "OBAMA:", "ROMNEY:"]
debate = [""]
for turn in statements:
	## work with text of each p tag
	text = turn.get_text().encode("UTF-8")

	## if all upper case, then skip turn bc it is annotation or audience
	if text.isupper():
		continue

	## Three conditions:
	## 1) add new element to debate list if speaker is not in previous debate turn
	## 2) add text to previous debate turn and remove speaker name if speaker was in
	## previous debate turn
	## 3) add text to previous debate turn if no speaker was provided
	for p in participants:
		if p in text:
			if p not in debate[-1]:
				debate.append(text)
			else:
				debate[-1] += text.replace(p, "")
			break
	else:
		debate[-1] += text