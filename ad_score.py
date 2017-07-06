import sys
import webbrowser
import csv
import datetime
import os
import time

print('\n')
print('Sit back and relax. Grab a cup of coffee or tea.')
print('ad_score.py takes a few minutes to run.')
print('\n')

if len(sys.argv) < 5:
    if len(sys.argv) < 4:
        print('Please input a training file, a test file, and an output file as command line arguments.')
    print('You may also enter m, p, or zp as a fourth argument to indicate how missing values are imputed.')
    print('m indicates mean imputation and p indicates probability imputation. See readme.txt for more info.')
    print('ad_score.py defaults to z-score imputation.')
    print('zp indicates probability imputation ads for ads that have no z-score info.')
    print('This setting should be used for actual test files.')
    print('ad_score.py defaults to mean imputation for ads with no z-score info.')
    if len(sys.argv) < 4:
        exit()

if (len(sys.argv) > 4):
    if (sys.argv[4] != 'p') and (sys.argv[4] != 'm'):
        flag = 1
    else:
        flag = 0
else:
    flag = 1

#filename variables
#clicks_train contains 87,141,732
datafilein = sys.argv[1]
testfilein = sys.argv[2]
datafileout = sys.argv[3]
#prints the start time for the script for runtime monitoring
print(datetime.datetime.now().time())

#initialize list variables
display = {}
ad = {}

#In each case I use i as the iterator for the data files
#i[0] is the display_id
#i[1] is the ad_id
#i[2] is the clicked variable ('1' = clicked; '0' = not clicked)
with open(datafilein, 'r') as filedata:
    data = csv.reader(filedata)
    next(data, None)
    for i in data:
        #sums the number of ads in a display
        #key = display_id
        #display[key][0] = clicked ad for the display, initialized as ''
        #display[key][1] = number of ads in a display
        try:
            display[i[0]][1] += 1
        except KeyError:
            display[i[0]] = ['',1]
        #sums the number of times the ad appears in the training dataset
        #key = ad_id
        #ad[key][2] = number of times the ad appears in the training dataset
        try:
            ad[i[1]][2] += 1
        except KeyError:
            ad[i[1]] = [0,0,1,0,0]
        #if ad was clicked:
        if i[2] == '1':
            #key = display_id
            #display[key][0] = clicked ad for the display
            display[i[0]][0] = i[1]
            #sums the number of times the ad was clicked
            #key = ad_id
            #ad[key][1] = number of times the ad was clicked
            ad[i[1]][1] += 1

#scores the proportion of times the ad was clicked
#clicked/number of appearances
for key in ad:
    ad[key][0] = ad[key][1]/ad[key][2]

print("Cycle 1 complete")
print(datetime.datetime.now().time())

with open(datafilein, 'r') as filedata:
    data = csv.reader(filedata)
    next(data, None)
    for i in data:
        outof = display[i[0]][1]
        #sums the total number of ads that appeared with this ad throughout
        #the training data set
        #key = ad_id
        #ad[key][3] = total number of ads appeared with (think total marbles in the jar)
        ad[i[1]][3] += outof
        #produces the probability that the ad would never be picked
        #key = ad_id
        #ad_id[key][4] = probability that the ad would never be picked
        if ad[i[1]][4] == 0:
            ad[i[1]][4] = (outof-1)/outof
        else:
            ad[i[1]][4] *= (outof-1)/outof

minimum = 0

for key in ad:
    #if ad was never picked,
    #its score is negative(1-prob that it would never be picked)
    #this makes scores for ads that were never picked < 0
    #with negative scores further left on numberline for ads that appeared more often
    if ad[key][1] == 0:
        ad[key][0] = -(1-ad[key][4])
    #if the ad was picked at least once:
    #it's score is the proportion of times it was picked minus
    #the probability of being picked completely at random
    #key = ad_id
    #ad[key][0] = ad score
    #ad[key][2] = total ad appearances (number of blue marbles)
    #ad[key][3] = total number of ads appearing with this ad (total marbles in jar)
    else:
        ad[key][0] = ad[key][0] - (ad[key][2]/ad[key][3])
        #determines ad score with the lowest value for ads that one at least once
        if ad[key][0] < minimum:
            minimum = ad[key][0]

ad_score_sum = 0

for key in ad:
    if ad[key][1] != 0:
        #ads the absolute value of minimum to all ads that won at least once
        #this makes all ads that were clicked at least once >= 0
        ad[key][0] += abs(minimum)
    if ad[key][2] < 10:
        ad[key][0] = ad[key][0] * 0.65
    ad_score_sum += ad[key][0]

ad_score_avg = ad_score_sum/len(ad)
sumsq = 0

for key in ad:
    ad[key][0] = ad[key][0] - ad_score_avg
    sumsq += ad[key][0]**2

standard_dev = (sumsq/(len(ad)-1))**0.5

for key in ad:
    ad[key][0] = ad[key][0]/standard_dev

print(str(len(ad))+" Ads Scored")
print("Sum: " + str(ad_score_sum))
print("Average: " + str(ad_score_avg))
print(datetime.datetime.now().time())

if (len(sys.argv) > 4) and ((sys.argv[4] == 'p') or (sys.argv[4] == 'zp')):
    dictfilein = "prob_dict.csv"
    with open(dictfilein, 'r') as csv_file:
        reader = csv.reader(csv_file)
        denominator = dict(reader)
    print("Prob Denominator Dictionary Read")
    print(datetime.datetime.now().time())

if flag == 1:
    dictfilein = "z_dict.csv"
    with open(dictfilein, 'r') as csv_file:
        reader = csv.reader(csv_file)
        z_score = dict(reader)
    print("Z-Score Dictionary Read")
    print(datetime.datetime.now().time())

t_display = {}
zcount = 0
ucount = 0

#reads the test file
with open(testfilein, 'r') as filedata:
    data = csv.reader(filedata)
    next(data, None)
    for i in data:
        #ads that never appear in the training set are given an ad_score of 0
        try:
            ad[i[1]]
        except KeyError:
            if flag == 1:
                try:
                    ad[i[1]] = [float(z_score[i[1]]),0]
                    zcount += 1
                except KeyError:
                    if (len(sys.argv) > 4) and (sys.argv[4] == 'zp'):
                        ad[i[1]] = [((1/int(denominator[i[0]])) - ad_score_avg)/standard_dev,0]
                    else:
                        ad[i[1]] = [0,0]
                    ucount += 1
            elif sys.argv[4] == 'p':
                ad[i[1]] = [((1/int(denominator[i[0]])) - ad_score_avg)/standard_dev,0]
                ucount += 1
                zcount = 0
            else:
                ad[i[1]] = [0,0]
                ucount += 1
                zcount = 0
        #creates a list of tuples with (ad score, ad_id) for each display_id
        try:
            t_display[i[0]] += [(ad[i[1]][0],i[1])]
        except KeyError:
            t_display[i[0]] = [(ad[i[1]][0],i[1])]

print(str(ucount) + " unknown ads")
print(str(len(ad)) + " ads scored")
print(str(len(t_display)) + " test displays scored")
print(datetime.datetime.now().time())

display_list = []
for key in t_display:
    #sorts the list of tuples by ad score in decending order:
    #ads with higher scores are more likely to be clicked
    t_display[key] = [x[1] for x in sorted(t_display[key],reverse=True)]
    #creates a list of tuples with (display_id,list of sorted ads for display)
    display_list += [(int(key),t_display[key])]

#sorts the displays in acending order because
#displays became unordered in the dictionary
display_list = sorted(display_list)

print("displays sorted")
print(datetime.datetime.now().time())

headlist = ["display_id","ad_id"]
with open(datafileout, 'w', newline = '') as datafile:
        csvfile = csv.writer(datafile)
        #writes headers to file
        csvfile.writerow(headlist)
        for x in display_list:
            #writes each display line to file
            csvfile.writerow([x[0]," ".join(x[1])])
#prints the script run end time

print(datetime.datetime.now().time())

#filename variables
datafilein = 'validation_results.csv'
datafileout2 = 'validation_results2.csv'
#prints the start time for the script for runtime monitoring
print("Start Evaluation of " + datafileout)
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


with open(datafileout, 'r') as filedata:
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
    print('{:>2}'.format(str(key)) + ": " + '{:>8}'.format(str(perform[key])) +
        " " + '{:>6}'.format(perf_pct))
    perf_sum += perform[key]
    ap_at_n = (1/key)*perform[key]
    map12 += ap_at_n

map12 = round(map12/len(display),5)
pct_sum = round(pct_sum,3)
print('{:>12}'.format("------- ") + "-------")
print('{:>11}'.format(str(perf_sum)) + '{:>8}'.format(str(pct_sum)) + "\n")
print("MAP@12 score: " + str(map12) + "\n")
print("End Evaluation of " + datafileout)
print(datetime.datetime.now().time())

greater3 = 0
for i in range(4,13):
    try:
        greater3 += perform[i]
    except KeyError:
        continue

results = [datafileout,map12]
for i in range(1,4):
    #writes each display line to file
    try:
        results += [100*perform[i]/perf_sum]
    except KeyError:
        results += [0]

results += [100*greater3/perf_sum]

#headlist = ["Submission","MAP@12","1","2","3",">3"]

with open(datafilein, 'r') as filedata:
    data = csv.reader(filedata)
    with open(datafileout2, 'w', newline = '') as datafile:
        csvfile = csv.writer(datafile)
        #writes headers to file
        #csvfile.writerow(headlist)
        for i in data:
            lineinfo = []
            for ii in range(len(i)):
                lineinfo += [i[ii]]
            csvfile.writerow(lineinfo)
        csvfile.writerow(results)

os.remove(datafilein)
os.rename(datafileout2,datafilein)

new = 2
url = "validation.html"
webbrowser.open(url,new=new)

time.sleep(90)
