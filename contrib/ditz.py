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

        path = os.path.join(issuedir, Project.filename)
        if os.path.exists(path):
            self.project = self._read(path)
        else:
            raise DitzError("'%s' is not a Ditz issue directory" % issuedir)

    def __iter__(self):
        match = os.path.join(self.issuedir, Issue.template % "*")
        for path in glob.glob(match):
            yield self._read(path)

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

    print "Releases:"
    for release in ditz.project.releases:
        date = release.release_time 
        print "   %s [%s]" % (release.name,
                              date if date else "unreleased")

    print "Issues:"
    for issue in ditz:
        print "   %s [%s]" % (issue.title, issue.type)
