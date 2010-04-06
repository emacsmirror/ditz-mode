# Update version control status when issues are added or removed.

# To enable: copy this file to your ~/.ditz/hooks directory.

Ditz::HookManager::on :after_add do |project, config, issues|
  vcs_command config, "add", issues
end

Ditz::HookManager::on :after_delete do |project, config, issues|
  vcs_command config, "remove", issues
end

Ditz::HookManager::on :after_update do |project, config, issues|
  # Nothing to do here.
end

def vcs_command config, cmd, issues
  if    FileTest.directory?(".hg")
    vcs = "hg -q"
  elsif FileTest.directory?(".svn")
    vcs = "svn -q"
  elsif FileTest.directory?("CVS")
    vcs = "cvs -q"
  else
    return
  end

  if config.issue_dir
    cmd = vcs + " " + cmd

    issues.each do |issue|
      cmd += " " + config.issue_dir + "/issue-" + issue.id + ".yaml"
    end

    system(cmd) or raise Error, "cannot execute command: #{cmd.inspect}"
  end
end
