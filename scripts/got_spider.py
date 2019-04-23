import scrapy
import pandas as pd

columns = ["CHARACTER", "UNIVERSE", "TITLE", "RELEASE_DATE"]

class movieSpider(scrapy.Spider):
    name = "got"
    start_urls = [
        'https://www.imdb.com/title/tt1480055/' # ep 1
    ]

    def parse(self, response):

        season = response.css('div.bp_heading::text').extract_first().strip()
        if (season != "Season 8"):

            # access people (leads in to parse2)
            link = response.css('div.credit_summary_item a::attr(href)')[-1].extract()
            if link is not None:
                link = response.urljoin(link)
                yield scrapy.Request(link, callback = self.parse2)

            # access next episode (calls back to parse)
            next_ep = response.css('a.bp_item.np_next::attr(href)').extract_first()
            if next_ep is not None:
                next_ep = response.urljoin(next_ep)
                yield scrapy.Request(next_ep, callback = self.parse)
        

    def parse2(self, response):
        name = response.xpath('//div[@class="parent"]/h3/a/text()').extract_first().strip()  # new title location
        filename = 'game_of_thrones/ep-%s.csv' % name.replace(" ", "")

        char_list = response.xpath('//td[@class="character"]/a/text()').extract()
        minors = response.xpath('//td[@class="character"]/text()').extract()

        for m in range(0, len(minors)):
            minors[m] = minors[m].strip()
            minors[m] = minors[m].replace("\n","")
            if "uncredited" in minors[m]:
                minors[m] = ""
        minors = list(filter(None, minors))
        
        movie_df = pd.DataFrame(columns=columns)
        movie_df["CHARACTER"] = char_list+minors
        movie_df["UNIVERSE"] = "GoT"
        movie_df["TITLE"] = name
        movie_df.to_csv(filename, header=True, index = False)

        # access dates (leads in to parse 3)
        link = response.css('span.nobr a::attr(href)').extract_first()
        if link is not None:
            link = response.urljoin(link)
            yield scrapy.Request(link, callback = self.parse3)

    def parse3(self, response):
        name = response.xpath('//div[@class="parent"]/h3/a/text()').extract_first().strip()  # new title location
        filename = 'game_of_thrones/ep-%s.csv' % name.replace(" ", "")

        date = response.xpath('//td[@class="release_date"]/text()').extract_first()

        movie_df = pd.read_csv(filename)
        movie_df["RELEASE_DATE"] = date
        
        movie_df.to_csv(filename, header=True, index = False)
            
