import csv
import matplotlib.pyplot as plt
import numpy as np

x,y = [],[]
a,b= [],[]
c,d= [],[]
csv_reader = csv.reader(open('data.csv'), delimiter=';')

i=0
for line in csv_reader:
    if i%3==0:
        x.append(float(line[1]))
        y.append(int(line[0]))
    if i%3==1:
        a.append(float(line[1]))
        b.append(int(line[0]))
    if i%3==2:
        c.append(float(line[1]))
        d.append(int(line[0]))
    i=i+1

plt.plot(y, x, 'o')

plt.plot(b, a , 'o')

plt.plot(d, c , 'o')

mn,mx =min(min(x),min(min(a),min(c))),max(max(x),max(max(a),max(c)))
plt.ylim([mn-mn,mx+mn ])

plt.show()
