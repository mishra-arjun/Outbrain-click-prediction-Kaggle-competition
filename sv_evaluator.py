import csv
import datetime

#filename variables
svfilein = 'xg1_subm.csv'
testfilein = 'clicks_train_joined9.csv'
#prints the start time for the script for runtime monitoring
print("Start Evaluation of " + svfilein)
print(datetime.datetime.now().time())
print("\r")

display = {}

#i[0] is the display_id
#i[1] is the ad_id
#i[2] is the clicked variable ('1' = clicked; '0' = not clicked)
with open(testfilein, 'r') as filedata:
    data = csv.reader(filedata)
    next(data, None)
    for i in data:
        if i[2] == '1':
            display[i[0]] = [i[1],[]]


with open(svfilein, 'r') as filedata:
    data = csv.reader(filedata)
    next(data, None)
    for i in data:
        display[i[0]][1] = i[1].split()

perform = {}

for key in display:
    position = display[key][1].index(display[key][0]) + 1
    try:
        perform[position] += 1
    except KeyError:
        perform[position] = 1

perf_sum = 0
pct_sum = 0
map12 = 0

print('{:4}'.format("Loc ") + '{:>7}'.format("Freq") + '{:>7}'.format("Pct"))
for key in perform:
    perf_pct = round((perform[key]/len(display))*100,3)
    pct_sum += perf_pct
    perf_pct = str(perf_pct)
    dec_loc = len(perf_pct) - (perf_pct.find(".") + 1)
    if perf_pct.find(".") == -1:
        perf_pct = perf_pct + ".000"
    elif dec_loc != 3:
        perf_pct = perf_pct + ("0" * (3-dec_loc))
    print('{:>2}'.format(str(key)) + ": " + '{:>7}'.format(str(perform[key])) +
        " " + '{:>6}'.format(perf_pct))
    perf_sum += perform[key]
    ap_at_n = (1/key)*perform[key]
    map12 += ap_at_n

map12 = round(map12/len(display),5)
pct_sum = round(pct_sum,3)
print('{:>12}'.format("------- ") + "------")
print('{:>11}'.format(str(perf_sum)) + '{:>7}'.format(str(pct_sum)) + "\n")
print("MAP@12 score: " + str(map12) + "\n")
print("End Evaluation of " + svfilein)
print(datetime.datetime.now().time())
