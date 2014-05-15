import csv

# Read in a csv and replace ? with NA.
# Also ignore entires without final exam scores

newcsv = []
with open('combinedb1.csv', 'r') as datafile:
    data = csv.reader(datafile)
    for row in data:
        if row[len(row)-1] != '?':
            for i in range(len(row)):
                if row[i] == '?':
                    row[i] = 'NA'
            newcsv.append(row)


with open('bloomNA1.csv', 'w') as writefile:
    writer = csv.writer(writefile)
    writer.writerows(newcsv)