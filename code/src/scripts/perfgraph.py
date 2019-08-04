
# coding: utf-8

# In[43]:


import statistics
import random
import matplotlib.pyplot as plt
# import seaborn
import re


plt.rc('text', usetex=True)


# In[44]:


# 12 queens

# test1_pho = [31.05, 30.87, 30.89, 30.90, 31.36]
# test1_swi = [7.84, 7.96, 7.78, 7.87, 7.86]
# test1_gnu = [2,2,2,2,2] #fakenews

test1_pho=[31.98, 31.43, 30.89, 31.58, 30.94]
test1_swi=[8.12, 7.75, 7.76, 7.78, 7.83]
test1_gnu=[20.64/10, 20.87/10, 21.62/10, 20.57/10, 20.6/10]


print(statistics.mean(test1_pho)/statistics.mean(test1_gnu))


# In[45]:


# quick5000 

test2_pho = [35.69, 35.81, 35.48, 35.70, 35.45]
test2_swi = [8.47, 8.67, 8.57, 8.53, 8.54]
test2_gnu = [0,0,0,0,0] #fakenews



print(statistics.mean(test2_pho)/statistics.mean(test2_swi))


# In[77]:


# hannoi_pho = [1.14, 1.13, 1.08, 1.11, 1.30]
# hannoi_swi = [28.00, 27.69, 28.47, 29.18, 29.82]
# hannoi_gnu = [3.00/100, 3.03/100, 2.53/100, 2.86/100, 2.74/100]


hannoi_pho = [0.49, 0.51, 0.49, 0.52, 0.45]
hannoi_swi = [26.16, 25.99, 26.16, 26.23, 25.98]
hannoi_gnu = [2.26/100, 2.21/100, 2.19/100, 2.38/100, 2.30/100]


print(statistics.mean(hannoi_pho)/statistics.mean(hannoi_gnu))


# In[74]:


col_pho = [4.62, 4.53, 4.54, 4.56, 4.55]
col_swi = [0.40, 0.41, 0.39, 0.40, 0.41]
col_gnu = [1.60/10, 1.61/10, 1.61/10, 1.59/10, 1.58/10]


# In[75]:


ack_pho=[3.48, 3.72, 4.23, 3.34, 3.24]
ack_swi=[83.32, 83.93, 84.53, 90.85, 84.11]
ack_gnu=[7.42/100, 7.35/100, 7.86/100, 7.35/100, 7.52/100]

print(statistics.mean(ack_pho)/statistics.mean(ack_gnu))


# In[79]:


f, (ax1, ax2, ax3,ax4,ax5) = plt.subplots(1, 5,figsize=(12, 3.5))
f.tight_layout()

def bar(means, labels, err, ax, title,log,label):
    x_coords = list(range(1, len(means)+1))
    ax.grid(axis='y',which='both',zorder=0)
    ax.set_axisbelow(True)
    pl = ax.bar(x_coords, means,yerr=err,capsize=6,color=['#76cc90', '#ea7272','#60bcea'],tick_label=labels)
   # ax.bar(x_coords, means,yerr=err,capsize=6,color="Spectral",tick_label=labels)
    ax.set_title(title)
    if log:
        ax.set_yscale('log')
    ax.set_ylabel(label)
    print(pl)
    return pl
    
                
#     ax.xticks(x_coords, labels)

def bardata(pho, swi, gnu, axis, title,log,label):
    means1 = [statistics.mean(x) for x in [pho, swi, gnu]]
    labels1 = ['Phoebe', 'Swi','GNU']
    err1 = [5 * statistics.stdev(x) for x in [pho, swi, gnu]]
    return bar(means1,labels1,err1, axis, title,log,label)

bardata(test1_pho,test1_swi,test1_gnu,ax1,'12 Queens',False,'Execution time / s')
p2 = bardata(test2_pho,test2_swi,test2_gnu,ax2,'Quick 5000',False,'')
bardata(col_pho,col_swi,col_gnu,ax3,'Graph colouring',True,'')
bardata(hannoi_pho,hannoi_swi,hannoi_gnu,ax4,'Hannoi',True,'')
bardata(ack_pho,ack_swi,ack_gnu,ax5,'Ack',True,'')


print(p2)

# def autolabel(plot):
#     for idx,rect in enumerate(plot):
#         height = rect.get_height()
#         ax.text(rect.get_x() + rect.get_width()/2., 1.05*height,
#                 "hi",
#                 ha='center', va='bottom', rotation=90)

# autolabel(ax2)


# ax2.text(rect.get_x() + rect.get_width()/2., 1.05*height,"hi",ha='center', va='bottom')

def autolabel(plt,ax):
    count = 0
    for idx,rect in enumerate(plt):
        if count == 2:
            height = rect.get_height()
            ax.text(rect.get_x() + rect.get_width()/2., 1,
                    "Compilation failure",
                    ha='center', va='bottom', rotation=90,size=12,backgroundcolor="white")
        count+=1

autolabel(p2,ax2)

