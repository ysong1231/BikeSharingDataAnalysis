#!/usr/bin/env /Users/mac/anaconda2/envs/py36/bin/python
# coding: utf-8

"""
Author: Yilin Song, syilin1231@gmail.com
Date: 2019-03
"""
import csv

dic_start = {}
dic_end = {}

with open("./Data/trip.csv","r",encoding="utf-8") as csvfile:
    read = csv.reader(csvfile)
    for i in read:
        date, start_station, end_station = i[-1], i[5], i[6]
        key_start = ','.join([date, start_station])
        key_end = ','.join([date, end_station])
        if key_start in dic_start:
            dic_start[key_start][0] += 1
        else:
            dic_start[key_start] = [1, i[17]] + i[30:50]
        if key_end in dic_end:
            dic_end[key_end][0] += 1
        else:
            dic_end[key_end] = [1, i[17]] + i[30:50]

with open("./Data/predic_start.csv","w",newline="") as datacsv:
    csvwriter = csv.writer(datacsv,dialect = ("excel"))
    for i in dic_start:
        csvwriter.writerow(i.split(',') + dic_start[i])

with open("./Data/predic_end.csv","w",newline="") as datacsv:
    csvwriter = csv.writer(datacsv,dialect = ("excel"))
    for i in dic_end:
        csvwriter.writerow(i.split(',') + dic_end[i])