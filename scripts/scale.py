import pandas as pd
import csv
weights = {"guy": 200, "creature":400, "building":100000000, "vehicle":10000, "object":50, "soldier":200}
people = dict()

movies = ["power-IronMan(2008)", "power-TheIncredibleHulk(2008)", "power-IronMan2(2010)", "power-Thor(2011)",
          "power-CaptainAmerica_TheFirstAvenger(2011)", "power-TheAvengers(2012)", "power-IronMan3(2013)",
          "power-Thor_TheDarkWorld(2013)", "power-CaptainAmerica_TheWinterSoldier(2014)", "power-GuardiansoftheGalaxy(2014)",
          "power-Avengers_AgeofUltron(2015)", "power-Ant-Man(2015)", "power-CaptainAmerica_CivilWar(2016)",
          "power-DoctorStrange(2016)", "power-GuardiansoftheGalaxyVol_2(2017)",
          "power-SpiderManHomecoming(2017)", "power-ThorRagnarok(2017)", "power-BlackPanther(2018)", "power-Avengers_InfinityWar(2018)",
          "power-Ant-ManandtheWasp(2018)"]

for film in movies:
    df = pd.read_csv("Marvel_Annotations/"+film+".csv")
    for index, row in df.iterrows():
        winner = row['vanquishing'].strip()
        loser = row['vanquished'].strip()
        #print(row)
        if winner in weights:
            if loser in weights:
                weights[winner] = (weights[winner] + weights[loser])
            else:
                weights[winner] = weights[winner] + 200
        else:
            if loser in weights:
                weights[winner] = weights[loser]
            else:
                weights[winner] = 200

        if winner in people:
            if loser in people[winner]:
                people[winner][loser] = people[winner][loser] + 1
            else:
                people[winner][loser] = 1
        else:
            people[winner] = {loser: 1}

#print(people)
print("Max generic guys killed")
max = 0
maxKills = 'no one'
for k, v in people.items():
    if 'guy' in v:
        if v['guy'] > max:
            max = v['guy']
            maxKills = k
print(max, maxKills)

print("\nmost property damage:")
max = 0
maxDamage = 'no one'
for k, v in people.items():
    if 'building' in v:
        if v['building'] > max:
            max = v['building']
            maxKills = k
print(max, maxKills)

print("\nMost vehicles totaled:")
max = 0
maxDamage = 'no one'
for k, v in people.items():
    if 'vehicle' in v:
        if v['vehicle'] > max:
            max = v['vehicle']
            maxKills = k
print(max, maxKills)


with open('strength.csv', 'w') as csv_file:
    writer = csv.writer(csv_file)
    for k, v in weights.items():
        writer.writerow([k, v])


          # where tf is civil war?
