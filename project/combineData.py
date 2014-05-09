import csv

# Read in all the data and combine it, using only the question scores (correct or incorrect) and ignoring the question answers.
# Gives 0 if they did not answer that question or do not appear in the dataset.

header = ["coursera_user"] #A row containing the headers for all the useful data points
alldata = {} #A dict where the key is the user and the value is a list of scores
newcsv = {}
for quiz in range(9):
    if quiz < 8:
        datafile = open('week'+str(quiz+1)+'quiz1.csv', 'r')
    else:
        datafile = open('finalexam.csv', 'r')
    data = csv.reader(datafile)
    first = True
    valid = [] #An array of the same length as the row with a 1 if the data is useful and a 0 if it is not
    id_pos = 4
    for row in data:
        val = 0
        for i in range(len(row)):
            if first:
                if "user_id" in row[i]:
                    id_pos = i
                if "_corr" in row[i]:
                    valid.append(True)
                    header.append("Q"+str(quiz)+row[i])
                else:
                    valid.append(False)
            else:
                if quiz<8:
                    if i == id_pos:
                        if int(row[i]) not in alldata: #Appends values for people that do not appear in data as well! (in a somewhat hacky fashion)
                            alldata[int(row[i])] = ['?' for x in range(200)]
                    if valid[i]:
                        alldata[int(row[id_pos])][quiz*20 + val] = (row[i])
                        val += 1
                else:
                    if i == id_pos:
                        if int(row[i]) not in alldata: #Appends only people taking the final exam
                            newcsv[int(row[i])] = ['?' for x in range(200)]
                        else:
                            newcsv[int(row[i])] = alldata[int(row[i])]
                    if valid[i]:
                        newcsv[int(row[id_pos])][quiz*20 + val] = (row[i])
                        val += 1
        first = False

writefile = open('combinedw?.csv', 'w')
writer = csv.writer(writefile)
writer.writerow(header)
for uid, scores in newcsv.iteritems():
    writer.writerow([uid]+scores)