"""
Python interface to Ditz issue directory.
"""

import os
import yaml
import glob

_ditz_tag = "ditz.rubyforge.org,2008-03-06"

class Ditz(object):
    def __init__(self, issuedir = "issues"):
        self.issuedir = issuedir

        # Read the project file.
        path = os.path.join(issuedir, Project.filename)
        if os.path.exists(path):
            self.project = self._read(path)
        else:
            raise DitzError("'%s' is not a Ditz issue directory" % issuedir)

        # Read the issues.
        self.issues = []
        match = os.path.join(issuedir, Issue.template % "*")
        for path in glob.glob(match):
            issue = self._read(path)
            self.issues.append(issue)

    def _read(self, path):
        fp = open(path)
        data = yaml.load(fp)
        fp.close()

        return data

    def _write(self, data, path):
        fp = open(path, "w")
        data = yaml.dump(data, fp, default_flow_style = False)
        fp.close()

class Project(yaml.YAMLObject):
    yaml_tag = u'!%s/project' % _ditz_tag
    filename = "project.yaml"

class Component(yaml.YAMLObject):
    yaml_tag = u'!%s/component' % _ditz_tag

class Release(yaml.YAMLObject):
    yaml_tag = u'!%s/release' % _ditz_tag

class Issue(yaml.YAMLObject):
    yaml_tag = u'!%s/issue' % _ditz_tag
    template = "issue-%s.yaml"

    @property
    def filename(self):
        return self.template % self.id

class DitzError(Exception): pass

if __name__ == "__main__":
    ditz = Ditz()

    print ditz.project.name
    print "Issues:"
    for issue in ditz.issues:
        print "   %s [%s]" % (issue.title, issue.type)
