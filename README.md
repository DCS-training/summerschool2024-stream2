# CDCS Summer School 2024 
# Text & Data Analysis in the Wild

Welcome to the CDCS Summer School Stream 2 Repository. Here you will be able to access all the material, data, and instructions connected to the school. 

Material, data and instructions will be published within this repository by Monday June 10th before the start of the course.

This course is designed to help researchers with coding experience understand how data and text analysis projects are performed in a research environment. 
It starts with identifying a series of research questions connected to this year’s core topic (Living in Scotland Past and Present). Then it explores how computational methods can be used to obtain, clean, and analyse structured and unstructured datasets in R to answer those questions. 

Topics will include **web scraping**, **text analysis**, **sentiment analysis**, **data wrangling**, **statistics**, and **data visualisation**.

## Key principles: 
- General knowledge of the interface of RStudio and coding is required. 
- Fosters interdisciplinary thinking by bringing social science and humanities researchers together to explore methods.
- Covers both structured and unstructured data.
- Illustrates the development process of data-led projects by moving through phases of the project lifecycle.
- Challenge-led, helping researchers learn how to deal with real-world data.

## RStudio Refresher
Although the Text and Data Analysis in the Wild stream will provide attendees with many new digital skills, a level of prior knowledge is necessary to get the most out of our training. Try our short quiz to test your knowledge of R, covering some of the basics you should be familiar with before the Summer School. 

[TAKE The Quiz](https://forms.office.com/e/cjsdkpbyMv)

If you didn't do as well as you'd hoped, don't worry! [In this video](https://edin.ac/3JzOM0P), Training Manager Dr. Lucia Michelin goes through a refresher of some of the core R tools and techniques.

## Content of the Repository
Each day of the Summer School has its own folder. Within each day, you will find the slides for the day and a folder for each block containing a data file/folder and the code that will be used during the class.


## Summer School Time Table
| |MONDAY|TUESDAY|WEDNESDAY|THURSDAY|FRIDAY|
|---|---|---|---|---|---|
|09:40-10:40|	Seminar|	Seminar|	Seminar|	Seminar|	Seminar|
|10:40-11:00|	Coffee Break|	Coffee Break|	Coffee Break|	Coffee Break|	Coffee Break|
|11:00-12:30|	Introduction|	Text Analysis|	Web Scraping|	Data Analysis|	Data Visualisation|
|12:30-13:30|	Lunch Break|	Lunch Break|	Lunch Break|	Lunch Break|	Lunch Break|
|13:30- 15:00|	Data Wrangling|	Text Analysis|	Sentiment Analysis|	Data Analysis|	Data Visualisation and Geographic Data|
|15:00-15:30|	Coffee Break|	Coffee Break|	Coffee Break|	Coffee Break|	Coffee Break|
|15:30-17:00|	Keynote Lecture|	BYOD|	BYOD|	BYOD|	Next Steps|
|Evening|Drinks Reception|Celidh Club| |Pub Quiz|Drinks|

## Summer School Format
o The Summer School is meant to be interactive, and you will be prompted to replicate what the instructor is demonstrating on your own machine.

o Besides the instructors, there will be helpers who are there to help you if you get stuck or if you run into an error. Please use your sticky notes to raise problems or issues and someone will help you with addressing it. During the introduction at the start of the Summer School, we will cover how to ask for help in more detail.

o We promote an inclusive and welcoming environment, so we ask you to be respectful towards our instructors, helpers and fellow participants. Disruptive behaviour will not be tolerated, and you will be asked to leave if it occurs. More information can be found in the Summer School Code of Conduct Document. 

## Summer School Data 

For the Whisky focused Data Wrangling, Null Hypothesis Testing, and Data Visualisation sessions of the workshop we will be working with the following datasets:

* [List of whisky distilleries in Scotland - Wikipedia](https://en.wikipedia.org/wiki/List_of_whisky_distilleries_in_Scotland)
* [2.2k+ Scotch Reviews - Compiled by Koki Ando - Kaggle](https://www.kaggle.com/datasets/koki25ando/22000-scotch-whisky-reviews?resource=download).

The Wikipedia dataset was scraped using the [Convert Wiki Tables to CSV](https://wikitable2csv.ggor.de/) webtool by [Gregor Weichbrodt](https://github.com/gambolputty/wikitable2csv). Whilst the Scotch Reviews dataset was originally sourced from [Whisky Advocate](https://whiskyadvocate.com/). 

Through these datasets, we will work on extracting numerical data from unstructured text (i.e., the Whisky ABV % from the whisky name description), other tidying methods, and on using "Fuzzy Join" methods to bind datasets. From there we will use Null Hypothesis Testing to investigate what factors of the dataset influence the review scores given to each Whisky. The use of the dataset will finish with data visualisation methods, to communicate our findings in informative, interesting, and eye-catching ways.

For the Web scraping and Sentiment Analysis block, we are going to scrape and analyse the whisky reviews published on the [https://www.connosr.com/](https://www.connosr.com/) website.

For the History of Scotland-focused text analysis, we are going to explore the [Statistical Accounts of Scotland](https://collectionsmanager.is.ed.ac.uk/handle/10683/119269). 
More information on the dataset can be found on the [StatAccount Website](https://stataccscot.edina.ac.uk/static/statacc/dist/home). 
The ‘Old’ Statistical Account (1791-99), under the direction of ​Sir John Sinclair of Ulbster, and the ‘New’ Statistical Account (1834-45) are reports of life In Scotland during the XVIII and XIX centuries. ​

They offer uniquely rich and detailed parish reports for the whole of Scotland, covering a vast range of topics including agriculture, education, trades, religion and social customs.​

For the Geographical Data Visualisation section, we are going to use edited data from the [National Records of Scotland](https://www.nrscotland.gov.uk/statistics-and-data/geography/our-products/other-national-records-of-scotland-nrs-geographies-datasets/historic-civil-parishes-pre-1891)

## Setting up

### Setting up on Noteable 
1. Go to https://noteable.edina.ac.uk/login
2. Login with your EASE credentials
3. Select RStudio as a personal notebook server and press start
4. Go to File >New Project>Version Control>Git
5. Copy and Paste this repository URL [https://github.com/DCS-training/summerschool2024-stream2/](https://github.com/DCS-training/summerschool2024-stream2/) as the Repository URL
6. The Project directory name will filled in automatically but you can change it if you want your folder in Notable to have a different name
7. Decide where to locate the folder. By default, it will locate it in your home directory 
8. Press Create Project

Congratulations you have now pulled the content of the repository on your Notable server space the last thing you need to do is to install the packages not already installed in Noteable.
Now you can access every.R file in this repo


### Setting up on Posit
1. Go to https://posit.cloud/
2. Signup either via Gmail or GitHub
3. Go on New Project
4. New Project from Git Repository
5. Copy and Paste this repository URL [https://github.com/DCS-training/summerschool2024-stream2/](https://github.com/DCS-training/summerschool2024-stream2/) as the Repository URL
6. The Project directory name will filled in automatically


## Licence of the material
All the material collected here is covered by a CC-BY-NC 4.0 License

