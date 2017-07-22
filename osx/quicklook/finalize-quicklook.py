import subprocess
import sys
import re
import os
import shutil
import stat

def get_otool_libs(binary):
    output = subprocess.check_output(["otool", "-L", binary])
    output = output.replace("\t", "")
    output = re.sub(" \(.+\)", "", output)
    deps = output.splitlines()
    deps.pop(0) # the first line is just the name of the binary, followed by a colon...
    deps = filter(lambda dep: not(dep.endswith(os.path.basename(binary)) or dep.startswith('/usr/lib') or dep.startswith('/System') or dep == binary), deps)
    return deps

def get_deps(binary):
    deps = get_otool_libs(binary)
    rv = list(deps)
    for dep in deps:
        new_deps = get_deps(dep)
        if new_deps:
            rv.extend(new_deps)
    return rv

def update_paths(binary):
    deps = get_otool_libs(binary)
    for dep in deps:
        subprocess.call(["install_name_tool", "-change", dep, "@loader_path/../lib/" + os.path.basename(dep), binary]) 


main = sys.argv[1]

deps = get_deps(main)
deps = sorted(set(map(lambda dep: os.path.join(os.path.realpath(os.path.dirname(dep)), os.path.basename(dep)), deps)))

libdir = os.path.abspath(os.path.join(os.path.dirname(main), '..', 'lib'))

# create lib directory
try:
    os.mkdir(libdir)
except OSError:
    pass

# copy libraries and update the paths of the libraries they link against
for dep in deps:
    shutil.copy(dep, libdir) 
    new_lib = os.path.join(libdir, os.path.basename(dep))
    os.chmod(new_lib, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP | stat.S_IROTH)
    update_paths(new_lib)

# and do the same for the main executable
update_paths(main)

