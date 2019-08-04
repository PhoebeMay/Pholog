
#
# for i in range(10):
#     for j in range(10):
#         # print("add(n"+str(i))+",n"+str(j)+')')
#         m = i - j
#         if m >= 0:
#             n = str(m)
#         else:
#             n = "m"+str(abs(m))
#         print("sub(n" + str(i) + ",n" + str(j) + ",n"+ n + ").")

def mystr(m):
    if m >= 0:
        n = str(m)
    else:
        n = "m"+str(abs(m))
    return n

for i in range(-9,10):
    for j in range(-9,10):
        if i != j:
            print("notequals(n" + mystr(i) + ",n" + mystr(j) + ").")
