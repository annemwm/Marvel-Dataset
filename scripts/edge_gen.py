import pandas
import glob

path = 'dc/*.csv'
files = glob.glob(path)
for csv in files:
    data = pandas.read_csv(csv)
    chars = data['CHARACTER']
    edge_df = pandas.DataFrame(columns = ["NAME 1", "NAME 2"])
    for c in range(0, len(chars)):
        name1 = chars[c]
        #book = data["TITLE",c]
        if (c+1 < len(chars)):
            for d in range(c+1, len(chars)):
                #if (data["TITLE",d] != book):
                #    continue
                #else:
                name2 = chars[d]
                edge_df = edge_df.append({"NAME 1": name1, "NAME 2": name2},
                                     ignore_index = True)
    film = csv[csv.find("/")+1:csv.find(".")]
    film = "dc_edges/"+film+"_edges.csv"
    edge_df.to_csv(film, index = False)
