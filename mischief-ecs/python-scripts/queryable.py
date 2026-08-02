
def instance(n):
    res = ""
    res += "instance {-# OVERLAPPING #-} ("

    for i in range(n):
        res += "Queryable q" + str(i) + " o" + str(i)
        if i != n- 1:
            res += ", "

    res += ") => Queryable ("
    for i in range(n):
        res += "q" + str(i)
        if i != n - 1:
            res += ", "

    res += ") ("

    for i in range(n):
        res += "o" + str(i)
        if i != n - 1:
            res += ","

    res += ") where \n"

    res += "  runQueryEntity ("

    for i in range(n):
        res += "q" + str(i)
        if i != n-1:
            res += ", "

    res += ") world entity = do \n"

    for i in range(n):
        res += "    r" + str(i) + " <- runQueryEntity q" + str(i) + " world entity\n"

    res += "\n    return $ ("

    for i in range(n - 1):
        res += ","

    res += ") <$> "

    for i in range(n):
        res += "r" + str(i)
        if i != n - 1:
            res += " <*> "

    res += "\n\n  runQueryInternal ("

    for i in range(n):
        res += "q" + str(i)
        if i != n-1:
            res += ", "

    res += ") archetypes world = do\n"

    for i in range(n):
        res += "    r" + str(i) + " <- runQueryInternal q" + str(i) + " archetypes world\n"

    res += "\n    return $ map (\\("

    for i in range(n):
        if i == 0:
            res += "(e0, b0, r0), "
        else:
            res += "(_, b" + str (i) + ", r" + str(i) + ")"
            if i != n - 1:
                res += ", "

    res += ") -> (e0, "

    for i in range(n):
        res += "b" + str (i)
        if i != n-1:
            res += " || "

    res += ", ("
    for i in range(n):
        res += "r" + str(i)
        if i != n - 1:
            res += ", "

    res += "))) $ getZipList $ ("
    for i in range(n - 1):
        res += ","

    res += ") <$>"

    for i in range(n):
        res += "ZipList r" + str(i)
        if i != n - 1:
            res += " <*> "

    res += "\n\n  queryTypes ("

    for i in range(n):
        res += "q" + str(i)
        if i != n-1:
            res += ", "

    res += ") = Set.unions["

    for i in range(n):
        res += "queryTypes q" + str (i)
        if i != n-1:
            res += ", "

    res += "]\n\n"

    return res


if __name__ == '__main__':
    res = ""

    for i in range(2, 16):
        res += instance(i)

    with open("queryable.txt", 'w') as f:
        f.write(res)