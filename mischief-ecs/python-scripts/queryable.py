

def instance(n):
    res = ""
    res += "instance {-# OVERLAPPING #-} ("

    for i in range(n):
        res += "Queryable q" + str(i)
        if i != n-1:
            res += ", "

    res += ") => Queryable ("
    for i in range(n):
        res += "q" + str(i)
        if i != n-1:
            res += ", "

    res += ") where \n"

    res += "  type QueryOutput ("
    for i in range(n):
        res += "q" + str(i)
        if i != n-1:
          res += ", "
    res += ") = ("

    for i in range(n):
        res += "QueryOutput q" + str(i)
        if i != n-1:
            res += ", "

    res += ")\n\n  runQueryEntity _ world entity = do \n"

    for i in range(n):
        res += "    r" + str(i) + " <- runQueryEntity (Proxy @q" + str(i) + ") world entity\n"

    res += "\n    return $ ("

    for i in range(n-1):
        res += ","

    res += ") <$> "

    for i in range(n):
        res += "r" + str(i)
        if i != n-1:
            res += " <*> "

    res += "\n\n  runQueryInternal _ archetypes world = do\n"

    for i in range(n):
        res += "    r" + str(i) + " <- runQueryInternal (Proxy @q" + str(i) + ") archetypes world\n"

    res += "\n    return $ map (\\("

    for i in range(n):
        if i == 0:
            res += "(e0, r0), "
        else:
            res += "(_, r" + str(i) + ")"
            if i != n-1:
                res += ", "

    res += ") -> (e0, ("
    for i in range(n):
        res += "r" + str(i)
        if i != n-1:
            res += ", "

    res += "))) $ getZipList $ ("
    for i in range(n-1):
        res += ","

    res += ") <$>"

    for i in range(n):
        res += "ZipList r" + str(i)
        if i != n-1:
            res += " <*> "

    res += "\n\n"

    return res

if __name__ == '__main__':
    res = ""

    for i in range(2, 16):
        res += instance(i)

    with open("queryable.txt", 'w') as f:
        f.write(res)