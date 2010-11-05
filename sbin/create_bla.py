#!/usr/bin/python
"""
This script is used for opening the external method bla.py (zope) which I
use some times.
"""
import os, sys

cwd = os.path.abspath('.')
here = os.path.dirname(os.path.abspath(__file__))
bla_template = os.path.join(here, 'bla.py')

# search for parts/instance*/Extensions
buildout_dir = cwd.split('/')
while 1:
    if len(buildout_dir)<1:
        print 'could not find parts directory'
        sys.exit(0)
    if 'parts' in os.listdir('/'.join(buildout_dir)):
        # found parts directory
        break
    buildout_dir.pop()
buildout_dir = '/'.join(buildout_dir)
parts = os.path.join(buildout_dir, 'parts')

parts_files = os.listdir(parts)
parts_files.sort()
extensions = None
for file in parts_files:
    if file.startswith('instance'):
        p = os.path.join(parts, file, 'Extensions')
        if not os.path.isdir(p):
            os.mkdir(p)
        extensions = p
        break
if not extensions:
    print 'could not find instance*/Extensions directory'
    sys.exit(0)

bla_path = os.path.join(extensions, 'bla.py')
if not os.path.isfile(bla_path):
    os.system('cp %s %s' % (bla_template, bla_path))
print 'file:', bla_path
