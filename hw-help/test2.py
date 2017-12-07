
def populateLuckyDict (luckyM, luckyF, selected):
    selected = selected.strip("\n")
    inStudent = selected.split(';')
    gender = inStudent[-1]
    name = inStudent[0]

    if gender == "F":
        #luckyF,
        if name in luckyF.keys():
            count = luckyF[name] + 1
            luckyF[name] = count
        else:
            luckyF[name] = 1
    elif gender == "M":
        #luckyM,
        if name in luckyM.keys():
            count = luckyM[name] + 1
            luckyM[name] = count
        else:
            luckyM[name] = 1
    else:
        print ("Something wrong")
        print ("Gender\n")
        exit(1)

inStudent  = "Kimberely Matthias;1;F"
inStudent2 = "Andrew Li;2;M"

luckyM = {}
luckyF = {}

populateLuckyDict(luckyM ,luckyF, inStudent)
populateLuckyDict(luckyM ,luckyF, inStudent2)
populateLuckyDict(luckyM ,luckyF, inStudent2)
populateLuckyDict(luckyM ,luckyF, inStudent)
populateLuckyDict(luckyM ,luckyF, inStudent2)
populateLuckyDict(luckyM ,luckyF, inStudent)
populateLuckyDict(luckyM ,luckyF, inStudent2)


print("Now printing dictionary")
for k,v in luckyF.items():
    print(k,v)


print("Now printing dictionary")
for k,v in luckyM.items():
    print(k,v)
