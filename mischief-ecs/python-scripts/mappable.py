def instance(n):
    res = ""
    res += "instance {-# OVERLAPPING #-} ("

    for i in range(n):
        res += "Mappable flag a" + str(i) + " b" + str(i)
        if i != n-1:
            res += ", "

    res += ") => Mappable flag ("
    for i in range(n):
        res += "a" + str(i)
        if i != n-1:
            res += ", "

    res += ") ("
    for i in range(n):
        res += "b" + str(i)
        if i != n-1:
            res += ", "

    res += ") where \n  mapTuple ("

    for i in range(n):
        res += "a" + str(i)
        if i != n-1:
            res += ", "

    res += ") = ("

    for i in range(0, n):
        res += "mapTuple @flag a" + str(i)
        if i != n-1:
            res += ","

    res += ")\n\n"

    return res

if __name__ == '__main__':
    res = ""

    for i in range(2, 16):
        res += instance(i)

    with open("mappable.txt", 'w') as f:
        f.write(res)