import json, os, re, yaml

def logs(pat="."):
    os.system("tail -F log/bfe.log | grep \"%s\"" % pat)

def crash():
    os.system("tail -F log/crash.log")

def refactor_src(pat, rep, root): # pattern, replacement, root
    def refactor(tokens):
        path="/".join(tokens)
        for entry in os.listdir(path):
            newtokens=tokens+[entry]
            filename="/".join(newtokens)
            if os.path.isdir(filename):
                refactor(newtokens)
            elif filename.endswith("pyc"):
                pass
            else:
                text=file(filename).read()
                newtext=re.sub(pat, rep, text)
                newfilename=re.sub(pat, rep, filename)
                if (text!=newtext or
                    filename!=newfilename):
                    print newfilename
                    dest=file(newfilename, 'w')
                    dest.write(newtext)
                    dest.close()
    refactor([root])

