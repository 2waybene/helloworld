#Question 1
mySecretStoryFile = open("story_secret.txt", "r", encoding="utf-8")
myClearStoryFile = open("story_clear.txt", "w", encoding="utf-8")
allSecretStory = mySecretStoryFile.readlines()
allKeys = allSecretStory[-1]
del(allSecretStory[-1])
allKeysList = allKeys.split(' ')
del (allKeysList[-1])
finalKeysList = [int(i) for i in allKeysList]
print(finalKeysList)


for aKey in finalKeysList:
    for aStory in allSecretStory:
        for aChr in aStory:
            aLineStr = aLineStr + chr(ord(aChr) - aKey)
            myClearStoryFile.write(aLineStr+"\n")
    aLineStr = ''

print(aLineStr)

myClearStoryFile.close()
mySecretStoryFile.close()
    




