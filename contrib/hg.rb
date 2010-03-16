# Update Mercurial status when issues are added or removed.

# To enable: copy this file to your ~/.ditz/hooks directory.

Ditz::HookManager::on :after_add do |project, config, issues|
  hg_command config, "add", issues
end

Ditz::HookManager::on :after_delete do |project, config, issues|
  hg_command config, "remove", issues
end

Ditz::HookManager::on :after_update do |project, config, issues|
  # Nothing to do here.
end

def hg_command config, cmd, issues
  if config.issue_dir
    hgcmd = "hg " + cmd

    issues.each do |issue|
      hgcmd += " " + config.issue_dir + "/issue-" + issue.id + ".yaml"
    end

    system(hgcmd) or raise Error, "cannot execute command: #{hgcmd.inspect}"
  end
end
