"""
Python interface to Ditz issue tracker (http://ditz.rubyforge.org).

@bug: write/read issue produces ditz error: Time/String comparison, due to
Z being omitted from output.
"""

import os
import yaml
import glob

class Ditz(object):
    def __init__(self, basedir = None):
        # Look in current directory if required.
        if not basedir:
            basedir = os.getcwd()

        # Find and read the Ditz config file.
        path = find_file(basedir, Config.filename)
        self.config = read_yaml(path)

        # Extract the issue directory name.
        try:
            dirname = self.config.issue_dir
        except AttributeError:
            raise DitzError("'%s' does not define 'issue_dir'" % path)

        # Find the issue directory.
        self.issuedir = find_file(basedir, dirname)

        # Read the project file.
        path = os.path.join(self.issuedir, Project.filename)
        self.project = read_yaml(path)

    def __iter__(self):
        """Iterate over all issues."""

        match = os.path.join(self.issuedir, Issue.template % "*")
        for path in glob.glob(match):
            yield read_yaml(path)

class DitzError(Exception): pass

class DitzObject(yaml.YAMLObject):
    yaml_loader = yaml.SafeLoader
    ditz_tag = "!ditz.rubyforge.org,2008-03-06"

    def write(self, issuedir):
        path = os.path.join(issuedir, self.filename)
        write_yaml(self, path)

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

def read_yaml(path):
    """Read YAML data from a file."""
    return yaml.safe_load(open(path))

def write_yaml(data, path):
    """Write YAML data to a file."""

    fp = open(path, "w")
    data = yaml.dump(data, fp, default_flow_style = False)
    fp.close()

def find_file(basedir, filename):
    """Find a filename in or above the base directory."""

    curdir = basedir

    while True:
        path = os.path.join(curdir, filename)
        if os.path.exists(path):
            return path
        elif curdir != "/":
            curdir = os.path.split(curdir)[0]
        else:
            raise DitzError("can't find '%s' in or above '%s'"
                            % (filename, basedir))

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
    
    issue.write(".")

if __name__ == "__main__":
    _test()
