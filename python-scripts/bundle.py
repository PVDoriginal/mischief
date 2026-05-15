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

    res += ") where \n  bundleData("

    for i in range(n):
        res += "c" + str(i)
        if i != n-1:
            res += ", "

    res += ") = \n    let\n"
    for i in range(n):
        res += "      BundleData {types = types" + str(i) + ", components = components" + str(i) + "} = bundleData c" + str(i) + "\n"

    res += "    in\n      BundleData{\n"

    res += "        types = concat ["

    for i in range(n):
        res += "types" + str(i)
        if i != n-1:
            res += ", "

    res += "],\n"

    res += "        components = concat ["

    for i in range(n):
        res += "components" + str(i)
        if i != n-1:
            res += ", "

    res += "]\n      }\n\n"
    return res

if __name__ == '__main__':
    res = ""

    for i in range(2, 16):
        res += instance(i)

    with open("bundle.txt", 'w') as f:
        f.write(res)