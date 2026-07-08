

def instance(n):
    res = ""
    res += "instance {-# OVERLAPPING #-} ("

    for i in range(n):
        res += "QueryData a" + str(i)
        if i != n-1:
            res += ", "

    res += ") => QueryData ("
    for i in range(n):
        res += "a" + str(i)
        if i != n-1:
            res += ", "

    res += ") where \n  types _ = Set.unions ["

    for i in range(n):
        res += "types $ Proxy @a" + str(i)
        if i != n-1:
            res += ", "

    res += "]\n\n"

    return res

if __name__ == '__main__':
    res = ""

    for i in range(2, 16):
        res += instance(i)

    with open("query-data.txt", 'w') as f:
        f.write(res)