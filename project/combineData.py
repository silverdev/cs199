import csv

# Read in all the data and combine it, using only the question scores (correct or incorrect) and ignoring the question answers.
# Gives 0 if they did not answer that question or do not appear in the dataset.

header = ["coursera_user"] #A row containing the headers for all the useful data points
newcsv = {} #A dict where the key is the user and the value is a list of scores
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
                if i == id_pos:
                    if int(row[i]) not in newcsv: #Appends values for people that do not appear in data as well! (in a somewhat hacky fashion)
                        newcsv[int(row[i])] = ['?' for x in range(200)]
                if valid[i]:
                    newcsv[int(row[id_pos])][quiz*20 + val] = (row[i])
                    val += 1
        first = False
offset = 0
for i in range(3):
    writefile = open('combinedb'+str(i)+'.csv', 'w')
    writer = csv.writer(writefile)
    head = [header[0]]
    for q in range(8):
        head+=header[1+q*20+offset:1+q*20+offset+8-i]
    for q in range(8):
        if i==0:
            head+=header[1+q*5+160:2+q*5+161]
        elif i==1:
            head+=header[1+q*5+162:2+q*5+163]
        else:
            head.append(header[1+q*5+164])
    writer.writerow(head)
    for uid, scores in newcsv.iteritems():
        newrow = [uid]
        for q in range(8):
            newrow+=scores[q*20+offset:q*20+offset+8-i]
        for q in range(8):
            if i==0:
                newrow+=scores[q*5+160:q*5+162]
            elif i==1:
                newrow+=scores[q*5+162:q*5+164]
            else:
                newrow.append(scores[q*5+164])
        writer.writerow(newrow)
    offset += 8-i