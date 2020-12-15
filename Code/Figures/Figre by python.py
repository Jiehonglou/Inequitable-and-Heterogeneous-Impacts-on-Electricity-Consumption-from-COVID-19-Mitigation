#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 14 23:41:03 2020

@author: jiehonglou
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import seaborn as sns; sns.set()
import gzip
import shutil
import fnmatch
import seaborn as sns
import glob

os.chdir('Project/Covid') 

#%% Supplemental Figure 3
df=pd.read_csv('AZ_mobility_0304.csv')

sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="hour", y="pchour",
             hue="year",linewidth = 4,
             data=df)
ax.axvline(x=5 ,color="grey",linestyle="--")
ax.text(4.75, 55, '5am', ha='center', va='center',rotation='vertical')
ax.axvline(x=9 ,color="grey",linestyle="--")
ax.text(8.75, 55, '9am', ha='center', va='center',rotation='vertical')
ax.axvline(x=17 ,color="grey",linestyle="--")
ax.text(16.75, 55, '5pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=21 ,color="grey",linestyle="--")
ax.text(20.75, 55, '9pm', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Hour', ylabel='Percentage of staying at hone', title="Arizona: Percentage of staying at home between 2019 and 2020 \n Full March and April")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('AZmarchandapril', dpi=450) 


df=pd.read_csv('AZ_mobility0315.csv')
sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="hour", y="pchour",
             hue="year",linewidth = 4,
             data=df)
ax.axvline(x=5 ,color="grey",linestyle="--")
ax.text(4.75, 55, '5am', ha='center', va='center',rotation='vertical')
ax.axvline(x=9 ,color="grey",linestyle="--")
ax.text(8.75, 55, '9am', ha='center', va='center',rotation='vertical')
ax.axvline(x=17 ,color="grey",linestyle="--")
ax.text(16.75, 55, '5pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=21 ,color="grey",linestyle="--")
ax.text(20.75, 55, '9pm', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Hour', ylabel='Percentage of staying at hone', title="Percentage of staying at home in March between 2019 and 2020 \n Between March 1 - March 15")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('AZmarch0315', dpi=450) 


df=pd.read_csv('AZ_mobility0331.csv')
sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="hour", y="pchour",
             hue="year",linewidth = 4,
             data=df)
ax.axvline(x=6 ,color="grey",linestyle="--")
ax.text(5.75, 54, '6am', ha='center', va='center',rotation='vertical')
ax.axvline(x=14 ,color="grey",linestyle="--")
ax.text(13.75, 54, '2pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=19 ,color="grey",linestyle="--")
ax.text(18.75, 54, '7pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=22 ,color="grey",linestyle="--")
ax.text(21.75, 54, '10pm', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Hour', ylabel='Percentage of staying at hone', title="Percentage of staying at home in March between 2019 and 2020 \n Between March 16 - March 31")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('AZmarch0331', dpi=450) 


df=pd.read_csv('AZ_mobility0430.csv')
sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="hour", y="pchour",
             hue="year",linewidth = 4,
             data=df)
ax.axvline(x=5 ,color="grey",linestyle="--")
ax.text(4.75, 58, '5am', ha='center', va='center',rotation='vertical')
ax.axvline(x=9 ,color="grey",linestyle="--")
ax.text(8.75, 58, '9am', ha='center', va='center',rotation='vertical')
ax.axvline(x=17 ,color="grey",linestyle="--")
ax.text(16.75, 58, '5pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=21 ,color="grey",linestyle="--")
ax.text(20.75, 58, '9pm', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Hour', ylabel='Percentage of staying at hone', title="Percentage of staying at home between 2019 and 2020 \n Between April 1 - April 30")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('AZapril0430', dpi=450) 

#%% Supplemental Figure 4

df=pd.read_csv('IL_mobility_03.csv')
sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="hour1", y="pchour",linewidth = 4,
             hue="year",
             data=df)
ax.axvline(x=6 ,color="grey",linestyle="--")
ax.text(5.75, 55, '6am', ha='center', va='center',rotation='vertical')
ax.axvline(x=14 ,color="grey",linestyle="--")
ax.text(13.75, 55, '2pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=19 ,color="grey",linestyle="--")
ax.text(18.75, 55, '7pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=22 ,color="grey",linestyle="--")
ax.text(21.75, 55, '10pm', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Hour', ylabel='Percentage of staying at hone', title="Percentage of staying at home in March between 2019 and 2020 \n Full March")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('ILmarch', dpi=450) 


df=pd.read_csv('IL_mobility_0317.csv')
sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="hour1", y="pchour",linewidth = 4,
             hue="year",
             data=df)
ax.axvline(x=6 ,color="grey",linestyle="--")
ax.text(5.75, 56, '6am', ha='center', va='center',rotation='vertical')
ax.axvline(x=14 ,color="grey",linestyle="--")
ax.text(13.75, 56, '2pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=19 ,color="grey",linestyle="--")
ax.text(18.75, 56, '7pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=22 ,color="grey",linestyle="--")
ax.text(21.75, 56, '10pm', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Hour', ylabel='Percentage of staying at hone', title="Percentage of staying at home in March between 2019 and 2020 \n Between March 1 - March 16")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('ILmarch0317', dpi=450) 


df=pd.read_csv('IL_mobility_0331.csv')
sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="hour1", y="pchour",linewidth = 4,
             hue="year",
             data=df)
ax.axvline(x=6 ,color="grey",linestyle="--")
ax.text(5.75, 55, '6am', ha='center', va='center',rotation='vertical')
ax.axvline(x=14 ,color="grey",linestyle="--")
ax.text(13.75, 55, '2pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=19 ,color="grey",linestyle="--")
ax.text(18.75, 55, '7pm', ha='center', va='center',rotation='vertical')
ax.axvline(x=22 ,color="grey",linestyle="--")
ax.text(21.75, 55, '10pm', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Hour', ylabel='Percentage of staying at hone', title="Percentage of staying at home in March between 2019 and 2020 \n Between March 17 - March 31")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('ILmarch0331', dpi=450) 



#%% Supplemental Figure 5

df=pd.read_csv('AZ_mobility_0304.csv')
sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="day", y="pchour",linewidth = 4,
             hue="year",
             data=df)
ax.axvline(x=1 ,color="k",linestyle="--")
ax.text(1.3, 54, '04/1 Stay-at-home order', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Date', ylabel='Percentage of staying at hone', title="Percentage of staying at home in April between 2019 and 2020")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('AZaprilbyday', dpi=450) 


sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="day", y="pchour",linewidth = 4,
             hue="year",
             data=df)
ax.axvline(x=16 ,color="k",linestyle="--")
ax.text(15.7, 52, '03/17 School close date', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Date', ylabel='Percentage of staying at hone', title="Percentage of staying at home in March between 2019 and 2020")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('AZmarchbyday', dpi=450) 

#%% Supplemental Figure 6

df=pd.read_csv('IL_mobility_03.csv')

sns.set(style="white")
plt.figure(figsize=(12,7))
ax=sns.lineplot(x="day", y="pchour",linewidth = 4,
             hue="year",
             data=df)
ax.axvline(x=21 ,color="k",linestyle="--")
ax.text(20.7, 47.5, '03/21 Stay-at-home order', ha='center', va='center',rotation='vertical')
ax.axvline(x=17 ,color="k",linestyle="--")
ax.text(16.7, 47.5, '03/17 School close date', ha='center', va='center',rotation='vertical')
ax.set(xlabel='Date', ylabel='Percentage of staying at hone', title="Percentage of staying at home in March between 2019 and 2020")
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.legend(["2019", "2020"])
plt.savefig('ILmarchbyday', dpi=450) 

