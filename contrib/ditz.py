"""
Python interface to Ditz issue tracker (http://ditz.rubyforge.org).
"""

import os
import yaml
import glob

class Ditz(object):
    def __init__(self, basedir = None):
        # Set base directory for searching.
        self.basedir = basedir if basedir else os.getcwd()

        # Find and read the Ditz config file.
        path = self._find(Config.filename)
        self.config = self._read(path)

        # Extract the issue directory name.
        try:
            dirname = self.config.issue_dir
        except AttributeError:
            raise DitzError("'%s' does not define 'issue_dir'" % path)

        # Find the issue directory.
        self.issuedir = self._find(dirname)

        # Read the project file.
        path = os.path.join(self.issuedir, Project.filename)
        self.project = self._read(path)

    def __iter__(self):
        """Iterate over all issues."""

        match = os.path.join(self.issuedir, Issue.template % "*")
        for path in glob.glob(match):
            yield self._read(path)

    def _find(self, filename):
        """Find a filename in or above the base directory."""

        curdir = self.basedir

        while True:
            path = os.path.join(curdir, filename)
            if os.path.exists(path):
                return path
            elif curdir != "/":
                curdir = os.path.split(curdir)[0]
            else:
                raise DitzError("can't find '%s' in or above '%s'"
                                % (filename, self.basedir))

    def _read(self, path):
        """Read YAML data from a file."""
        return yaml.safe_load(open(path))

    def _write(self, data, path):
        """Write YAML data to a file."""

        fp = open(path, "w")
        data = yaml.dump(data, fp, default_flow_style = False)
        fp.close()

class DitzError(Exception): pass

class DitzObject(yaml.YAMLObject):
    yaml_loader = yaml.SafeLoader
    ditz_tag = "!ditz.rubyforge.org,2008-03-06"

class Config(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/config'
    filename = ".ditz-config"

class Project(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/project'
    filename = "project.yaml"

class Component(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/component'

class Release(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/release'

class Issue(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/issue'
    template = "issue-%s.yaml"

    @property
    def filename(self):
        return self.template % self.id

def _test(basedir = None):
    ditz = Ditz(basedir)

    print "User: %s <%s>" % (ditz.config.name, ditz.config.email)

    print
    print "Project:", ditz.project.name

    print
    print "Releases:"
    for release in ditz.project.releases:
        date = release.release_time 
        print "   %s [%s]" % (release.name, date if date else "unreleased")

    print
    print "Issues:"
    for issue in ditz:
        print "   %s [%s]" % (issue.title, issue.type[1:])
    
if __name__ == "__main__":
    _test()
