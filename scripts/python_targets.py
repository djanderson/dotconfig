"""https://stackoverflow.com/a/68450405"""

from pprint import pprint

from pip._internal.models.target_python import TargetPython

target_python = TargetPython()

pep425tags = target_python.get_tags()

# Higher in the list takes priority
pprint(pep425tags)
