import json, os, re, yaml

def logs(pat="."):
    os.system("tail -F log/snp.log | grep \"%s\"" % pat)

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

"""
- this is a very basic check for imported keywords
- doesn't check for number of keywords / between foobar/1 and foobar/2, eg
"""

def check_imports(srcdir="src"):
    def parse_import(text):
        tokens=[tok for tok in re.split("\\W", text.replace(" ", ""))
                if (tok!='' and
                    not re.match("^\\d+$", tok))]
        return tokens[1], set(tokens[2:])
    def filter_imports(text):
        imports=set()
        for tok in text.split("\n"):
            if not tok.startswith("-import"):
                continue
            mod, fns = parse_import(tok)
            imports.update(fns)
        return imports
    def scrub_imports(text):
        return "\n".join([tok for tok in text.split("\n")
                          if not tok.startswith("-import")])
    def check(filename, errors):
        text=file(filename).read()
        cleantext=scrub_imports(text)
        imports=filter_imports(text)
        for fn in imports:
            if not re.search(fn, cleantext):
                errors.append("%s - %s/x unused" % (filename.split("/")[1], fn))
    errors=[]
    for filename in os.listdir(srcdir):
        check(srcdir+"/"+filename, errors)
    return errors
"""
- this is a basic check to see if #{app}.hrl is included but not used
"""

def check_includes(srcdir="src"):
    def filter_appname():
        tokens=[tok for tok in file("rel/vm.args").read().split("\n")
                if tok.startswith("-setcookie")]
        return tokens[0].split(" ")[-1]
    def has_headers(appname):
        return os.path.exists("include/%s.hrl" % appname)
    def parse_definition(text):
        tokens=[tok for tok in re.split("\\(|\\)|\\,|\\.",
                                        text.replace(" ", ""))
                if tok!='']
        return "?"+tokens[1]
    def filter_definitions(appname):
        text=file("include/%s.hrl" % appname).read()
        return [parse_definition(tok)
                for tok in text.split("\n")
                if tok.startswith("-define")]
    def validate(filename, appname, definitions):
        text=file(filename).read()
        if not ("-include(\"%s.hrl\")." % appname) in text:
            return True
        for definition in definitions:
            if definition in text:
                return True
        return False
    appname=filter_appname()
    if not has_headers(appname):
        return []
    definitions, errors = filter_definitions(appname), []
    for filename in os.listdir(srcdir):
        if validate(srcdir+"/"+filename, appname, definitions):
            continue
        errors.append("%s - %s.hrl unused" % (filename, appname))
    return errors

def check():
    errors=[]
    errors+=check_imports()
    errors+=check_includes()
    for error in errors:
        print error
