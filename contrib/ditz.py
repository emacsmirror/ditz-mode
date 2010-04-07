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

        path = os.path.join(self.issuedir, "project.yaml")
        if os.path.exists(path):
            self.project = self.read(path)
        else:
            raise DitzError("'%s' is not a Ditz issue directory" % issuedir)

    def issues(self):
        match = os.path.join(self.issuedir, "issue*.yaml")
        for path in glob.glob(match):
            yield self.read(path)

    def read(self, path):
        fp = open(path)
        data = yaml.load(fp)
        fp.close()

    def write(self, data, path):
        fp = open(path, "w")
        data = yaml.dump(data, fp, default_flow_style = False)
        fp.close()

class Project(yaml.YAMLObject):
    yaml_tag = u'!%s/project' % _ditz_tag

class Component(yaml.YAMLObject):
    yaml_tag = u'!%s/component' % _ditz_tag

class Release(yaml.YAMLObject):
    yaml_tag = u'!%s/release' % _ditz_tag

class Issue(yaml.YAMLObject):
    yaml_tag = u'!%s/issue' % _ditz_tag

    @property
    def filename(self):
        return "issue-%s.yaml" % self.id

class DitzError(Exception): pass

if __name__ == "__main__":
    ditz = Ditz()
    for issue in ditz.issues():
        print issue.title, issue.filename
