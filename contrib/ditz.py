"""
Python interface to Ditz issue tracker (http://ditz.rubyforge.org).

@todo: lazy reading of issues.

@bug: write/read issue produces ditz error: Time/String comparison, due to
Z being omitted from output.
"""

import os
import csv
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

    def write_csv(self, path, *attrs):
        """Write issues to CSV file."""

        if not attrs:
            attrs = Issue.attributes

        fp = open(path, "w")
        writer = csv.writer(fp)

        writer.writerow([attr.capitalize() for attr in attrs])
        for issue in sorted(self):
            writer.writerow([getattr(issue, attr) for attr in attrs])

        fp.close()

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
    attributes = ["name", "email", "issue_dir"]

class Project(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/project'
    filename = "project.yaml"
    attributes = ["name", "version", "components", "releases"]

class Component(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/component'

class Release(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/release'
    attributes = ["name", "status", "release_time", "log_events"]

    def __str__(self):
        return "%(name)s (%(release_time)s)" % self.__dict__

class Issue(DitzObject):
    yaml_tag = DitzObject.ditz_tag + '/issue'
    template = "issue-%s.yaml"
    attributes = ["title", "desc", "type", "component", "release",
                  "status", "disposition"]

    @property
    def filename(self):
        return self.template % self.id

    def __cmp__(self, other):
        return (cmp(self.status, other.status) or
                cmp(self.type, other.type) or
                cmp(self.title, other.title) or
                0)

    def __str__(self):
        return "%(title)s (%(type)s)" % self.__dict__

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
        print "   ", release

    print
    print "Issues:"
    for issue in sorted(ditz):
        print "   ", issue

    #ditz.write_csv("issues.csv", "title", "desc", "type", "release", "status")
    
if __name__ == "__main__":
    _test()
