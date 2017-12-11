
testKey = "6 9 10 5 8"
k = testKey.split(" ")
print(k)

counter = 12
k1 = k[:]
kT = k1.pop(0)



def decode (word, k):
    aLineStr = ''
    k1 = k[:]
    kT = k1.pop(0)
    for aChr in word:
        if (len(k1) > 0):
           # print (kT)
            aLineStr = aLineStr + chr(ord(aChr) - int(kT))
            kT = k1.pop(0)
        else:
          #  print(kT)
            aLineStr = aLineStr + chr(ord(aChr) - int(kT))
            k1 = k[:]
            kT = k1.pop(0)
    return(aLineStr)

line = decode ("This is very interesting", k)
print(line)


secretLine = "Trosylx&sy&ym{ohzhnowm*zpk)zy}nt|pku*vl%ouoml}nsl%f*kyskpvfysts*vl*ig|hww)rwtw}skl(isk%rxtv(rt%g%xf|zuozthuk*ho{m(kymf&zx&}rw})~rn)yxyojn'uk&}jvxv3%Xt%pi|2)"



