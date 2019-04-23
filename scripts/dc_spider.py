import scrapy
import pandas as pd

columns = ["CHARACTER", "UNIVERSE", "TITLE", "RELEASE_DATE"]

class movieSpider(scrapy.Spider):
    name = "dc"
    start_urls = [
        'https://www.imdb.com/title/tt0468569/fullcredits', # The Dark Knight
        'https://www.imdb.com/title/tt0372784/fullcredits', # Batman Begins,
        'https://www.imdb.com/title/tt1133985/fullcredits', # Green Lantern
        'https://www.imdb.com/title/tt1345836/fullcredits', # The Dark Knight Rises
        'https://www.imdb.com/title/tt0770828/fullcredits', # Man of Steel
        'https://www.imdb.com/title/tt2975590/fullcredits', # Batman v Superman: Dawn of Justice
        'https://www.imdb.com/title/tt1386697/fullcredits', # Suicide Squad
        'https://www.imdb.com/title/tt0451279/fullcredits', # Wonder Woman
        'https://www.imdb.com/title/tt0974015/fullcredits', # Justice League
        'https://www.imdb.com/title/tt1477834/fullcredits', # Aquaman
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
        movie_df["UNIVERSE"] = "DC"
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
