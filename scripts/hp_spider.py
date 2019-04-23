import scrapy
import pandas as pd

columns = ["CHARACTER", "UNIVERSE", "TITLE", "RELEASE_DATE"]

class movieSpider(scrapy.Spider):
    name = "mcu"
    start_urls = [
        'https://www.imdb.com/title/tt0241527/fullcredits', # Sorceror's Stone
        'https://www.imdb.com/title/tt0295297/fullcredits', # Chamber of Secrets
        'https://www.imdb.com/title/tt0304141/fullcredits', # Prisoner of Azkaban
        'https://www.imdb.com/title/tt0330373/fullcredits', # Goblet of Fire
        'https://www.imdb.com/title/tt0373889/fullcredits', # Order of the Phoenix
        'https://www.imdb.com/title/tt0417741/fullcredits', # Half-Blood Prince
        'https://www.imdb.com/title/tt0926084/fullcredits', # Deathly Hallows 1
        'https://www.imdb.com/title/tt1201607/fullcredits'  # Deathly Hallows 2
    ]

    def parse(self, response):
        name = response.css('title::text').extract_first()
        if (name.find(")")!=-1):
            name = name[0:name.find(")")+1]
            name = name.replace(" ","")
        filename = 'movie-%s.csv' % name

        cast_list = response.xpath('//table[@class = "cast_list"]/tr')[0]
        top_id = cast_list.xpath('//td[@class="character"]/a').extract()
        top_name = cast_list.xpath('//td[@class="character"]/a/text()').extract()

        # get mains, and fix doubles ie tony stark + iron man
        double = []
        adj = 0
        for i in range(0, len(top_id)):
            t = top_id[i]
            j = top_id[i-1]
            t = t[t.find('n'):t.find('?')]
            j = j[j.find('n'):j.find('?')]

            if t == j:
                double.append(i - 1 - adj)
                adj+=1
        for j in double:
            top_name[j] = " / ".join(top_name[j:j+2])
            del top_name[j+1]


        # get minor chars who may still be needed
        minors = response.xpath('//td[@class = "character"]/text()').extract()
        for m in range(0, len(minors)):
            minors[m] = minors[m].strip()
            minors[m] = minors[m].replace("\n","")
            if "uncredited" in minors[m]:
                minors[m] = ""
        minors = list(filter(None, minors))

        # init dataframe
        movie_df = pd.DataFrame(columns=columns)
        movie_df["CHARACTER"] = top_name+minors
        movie_df["UNIVERSE"] = "HP"
        movie_df["TITLE"] = name[0:name.find('(')]

        # save data
##        with open(filename, 'w') as f:
##            for item in top_name:
##                f.write("%s\n" % item)
##            for item in minors:
##                f.write("%s\n" % item)
        movie_df.to_csv(filename, header=True, index = False)

        ## access dates (leads in to parse2)
        link = response.css('span.nobr a::attr(href)').extract_first()
        if link is not None:
            link = response.urljoin(link)
            yield scrapy.Request(link, callback = self.parse2)



    def parse2(self, response):
        date = response.xpath('//td[@class="release_date"]/text()').extract_first() # first release date
        name = response.css('title::text').extract_first()
        name = name[0:name.find(')')+1]
        name = name.replace(" ","")
        filename = 'movie-%s.csv' % name

        date = response.xpath('//td[@class="release_date"]/text()').extract_first()

        movie_df = pd.read_csv(filename)
        movie_df["RELEASE_DATE"] = date

        movie_df.to_csv(filename, header=True, index = False)
