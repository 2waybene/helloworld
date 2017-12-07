#Question 4

#Fucntion for q4
def deEncryt (key1, key2, aStudent, outstream):
    counter = 0
    aLineStr=''
    for aChr in aStudent:
        counter = counter + 1
        if aChr == "\n":
            continue
        if (counter % 2 )== 0:
            aLineStr = aLineStr + chr(ord(aChr) - key2)
        else:
            aLineStr = aLineStr + chr(ord(aChr) - key1)
    outstream.write(aLineStr + "\n")


myStudent2SecretFile = open("students_secret.txt", "r", encoding="utf-8")
myStudent2ClearFile = open("students_clear.txt", "w", encoding="utf-8")
allSecret = myStudent2SecretFile.readlines()
myKeys = allSecret[-1]

##  Get my keys back
if len(myKeys) == 3:
    print (myKeys)
    if myKeys[0] == "1":
        myKey1 = "10"
        myKey2 = myKeys[2]
    else:
        myKey1 = myKeys[0]
        myKey2 = "10"
else:
    myKey1 = myKeys[0]
    myKey2 = myKeys[1]

myKey1 = int(myKey1)
myKey2 = int(myKey2)

##  Now, decoding..
allSecret = allSecret[:-1]
for aSecret in allSecret:
     if len(aSecret)>1:
         print(aSecret)
         deEncryt(myKey1,myKey2,aSecret,myStudent2ClearFile)



