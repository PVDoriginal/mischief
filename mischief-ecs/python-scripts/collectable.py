def instance(n):
    res = ""
    res += "instance {-# OVERLAPPING #-} (Semigroup s, "

    for i in range(n):
        res += "Collectable a" + str(i) + " s"
        if i != n-1:
            res += ", "

    res += ") => Collectable ("
    for i in range(n):
        res += "a" + str(i)
        if i != n-1:
            res += ", "

    res += ") s where \n  collect ("

    for i in range(n):
        res += "a" + str(i)
        if i != n-1:
            res += ", "

    res += ") = foldr (<>) (collect a0) ["

    for i in range(1, n):
        res += "collect a" + str(i)
        if i != n-1:
            res += ","

    res += "]\n\n"

    return res

if __name__ == '__main__':
    res = ""

    for i in range(2, 16):
        res += instance(i)

    with open("collectable.txt", 'w') as f:
        f.write(res)