

def instance(n):
    res = ""
    res += "instance {-# OVERLAPPING #-} ("

    for i in range(n):
        res += "Bundle c" + str(i)
        if i != n-1:
            res += ", "

    res += ") => Bundle ("
    for i in range(n):
        res += "c" + str(i)
        if i != n-1:
            res += ", "

    res += ") where \n  bundleData ("

    for i in range(n):
        res += "c" + str(i)
        if i != n-1:
            res += ", "

    res += ") = let\n"
    for i in range(n):
        res += "      BundleData {elements = set" + str(i) + "} = bundleData c" + str(i) + "\n"

    res += "    in BundleData $ Set.unions ["

    for i in range(n):
        res += "set" + str(i)
        if i != n-1:
            res += ", "

    res += "]\n\n"

    return res

if __name__ == '__main__':
    res = ""

    for i in range(2, 16):
        res += instance(i)

    with open("bundle.txt", 'w') as f:
        f.write(res)