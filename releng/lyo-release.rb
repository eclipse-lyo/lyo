#!/usr/bin/env ruby

require 'optparse'
require 'nokogiri'

# Laundry list:
# - check Git status in the repos-to-be-released
# - do the "git co master && git pull && git co -b r-%version%" dance
# - actually switch up the version
# - .m2 cleanup for current version
# - mvn clean install
# - mvn CVE scan
# - mvn html report & grep for version conflict
# - switch to http://whatisthor.com
#
#
# see http://www.pybloggers.com/2015/06/python-ruby-and-golang-a-command-line-application-comparison/

puts "ruby #{ RUBY_VERSION }p#{ RUBY_PATCHLEVEL } (tested with 2.3.0p0)"

mvn_version = %x{mvn -version}.lines[0].strip
mvn_path = %x{which mvn}
puts "Using #{mvn_version} at #{mvn_path} (tested with 3.5.4)\n"

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: version.rb [options]"

  opts.on("-vVERSION", "--version=VERSION", "Set version") do |v|
    options[:command] = :command_version
    options[:version] = v
  end

  opts.on("-i", "--install", "Run maven install") do
    options[:command] = :command_mvn_install
  end

  opts.on("--status", "Git status") do
    options[:command] = :command_git_status
  end

  opts.on("--reset-hard", "Reset Git repos to a clean state") do
    options[:command] = :command_reset_hard
  end

  opts.on("--ensure-clean", "Make sure every repo is clean") do
    options[:command] = :command_ensure_clean
  end

  opts.on("--ensure-fresh-master", "Make sure every repo is on master branch and is even with the remote") do
    options[:command] = :command_refresh_master
  end

  opts.on("--version-branch", "Create a branch before updating the version") do
    options[:branch] = true
  end

end.parse!


# p options
# p ARGV
puts "Scanning current versions"
parents = [ # ACHTUNG! Order matters
  { name: "Lyo Core",
    dir: "core/oslc4j-core-build/",
    repo: "core/" },
  { name: "Lyo Domains",
    dir: "domains/oslc-domains/",
    repo: "domains/" },
  { name: "Lyo Client v4",
    dir: "client/oslc4j-client/",
    repo: "client/" },  
  { name: "Lyo Client legacy",
    dir: "client/oslc-java-client/",
    repo: "client/" },
  { name: "Lyo Server",
    dir: "server/",
    repo: "server/" },
  { name: "Lyo Store",
    dir: "store/src/",
    repo: "store/" },
  { name: "Lyo TRS Server",
    dir: "trs-server/",
    repo: "trs-server/" },
  { name: "Lyo TRS Client",
    dir: "trs-client/",
    repo: "trs-client/" },
  { name: "Lyo Validation",
    dir: "validation/",
    repo: "validation/" },
  { name: "Lyo LDP",
    dir: "ldp/org.eclipse.lyo.ldp.build/",
    repo: "ldp/" }

  # TODO Designer
    # TODO Validation

    # TODO RIO
    # TODO Test Suite
]

# no_parent = {
#   :client => "client/org.eclipse.lyo.client.java/",
#   :client_sample => "client/org.eclipse.lyo.client.java.sample/",
#   :client_sample_oauth => "client/org.eclipse.lyo.client.java.oauth.sample/",
#   :docs_lab1 => "docs/Lab1/",
#   :docs_lab2 => "docs/Lab2/",
#   :docs_lab3 => "docs/Lab3/",
#   :docs_lab4 => "docs/Lab4/",
#   :docs_lab5 => "docs/Lab5/",
#   :docs_lab6 => "docs/Lab6/",
#   :docs...
#   :rio...
#   :server_samples...
# }

# deprecated = {
#   # TODO not java
#   :client_perl => "client/org.eclipse.lyo.client.perl/",
# }


##
# More than a magnitude faster than through mvn
def maven_version_xml(path)
  if path.end_with? ".xml"
    file = path
  else
    file = File.join(path, "pom.xml")
  end
  doc = File.open(file) { |f| Nokogiri::XML(f) }
  doc.remove_namespaces!
  begin
    doc.at_xpath("/project/version").content
  rescue => exception
    begin
      doc.at_xpath("/project/parent/version").content
    rescue => exception
      "NA!"
    end
  end
end


def print_all_versions
  Dir.glob('**/pom.xml').select{|f| !f["target/"]}.each do |pom|
    version = maven_version_xml(pom)
    puts "=> #{pom} v#{version}"
  end
end

def branch(dir, version)
  branch_name = "r-#{version}"
  puts "*"*79
  puts "=> Checking out a new branch '#{branch_name}' on the '#{dir}'"
  puts "*"*79

  puts %x( cd #{dir} && git checkout -B #{branch_name} )
end

print_all_versions()

if :command_version == options[:command]
  parents.each { |parent|
    if options[:branch]
      branch(parent[:dir], options[:version])
    end
    puts "*"*79
    puts "=> Setting the version on #{parent[:name]} and all its modules"
    puts "*"*79
    puts %x{cd #{parent[:dir]} && pwd && mvn versions:set -DnewVersion=#{options[:version]} -DprocessAllModules -DgenerateBackupPoms=false -DforceStdout -B}
  }

  # designer_dir = "designer/"

  # if options[:branch]
  #   branch(designer_dir, options[:version])
  # end
  # puts "Updating Lyo Tools version"
  # # puts %x( cd #{designer_dir} && pwd && versions:set -DnewVersion=#{options[:version]} -DgenerateBackupPoms=false -DforceStdout -B )
  # puts %x( cd #{designer_dir} && pwd && mvn -f pom.xml tycho-versions:set-version -DnewVersion=#{options[:version]} -DforceStdout -B )
  # # puts %x( cd #{designer_dir}/org.eclipse.lyo.tools.parent && pwd && mvn -f pom.xml tycho-versions:set-version -DnewVersion=#{options[:version]} -DforceStdout -B )

  print_all_versions()
end

if :command_mvn_install == options[:command]
  puts "Running 'mvn clean install' on each parent"
  parents.each {|parent|
    puts "*"*79
    puts "=> verifying '#{parent[:name]}'"
    puts "*"*79
    puts %x{ cd #{parent[:dir]} && pwd && mvn clean install -DforceStdout -B }
    if $?.exitstatus != 0
      fail "**mvn install has failed**"
    end
  }
end

if :command_git_status == options[:command]
  parents.reject { |parent| parent[:repo].nil? }
    .each { |parent|
      puts "*"*79
      puts "=> checking Git status on '#{parent[:name]}'"
      puts "*"*79
      puts %x( cd #{parent[:repo]} && pwd && git status )
      if $?.exitstatus != 0
        fail "**git status returned an error**"
      end
    }
end

if :command_reset_hard == options[:command]
  parents.reject { |parent| parent[:repo].nil? }
    .each { |parent|
      puts "*"*79
      puts "=> resetting Git status on '#{parent[:name]}'"
      puts "*"*79
      puts %x( cd #{parent[:repo]} && pwd && git reset --hard )
      if $?.exitstatus != 0
        fail "**git reset returned an error**"
      end
    }
end


if :command_ensure_clean == options[:command]
  parents.reject { |parent| parent[:repo].nil? }
    .each { |parent|
      puts "*"*79
      puts "=> checking Git status on '#{parent[:name]}'"
      puts "*"*79
      puts %x( cd #{parent[:repo]} && pwd )
      git_status = %x( cd #{parent[:repo]} && git status --porcelain )
      if git_status.lines.count >= 1
        fail "**ERROR: the repo is not clean**"
      end
    }
end

if :command_refresh_master == options[:command]
  parents.reject { |parent| parent[:repo].nil? }
    .each { |parent|
      puts "*"*79
      puts "=> checking Git status on '#{parent[:name]}'"
      puts "*"*79
      puts %x( cd #{parent[:repo]} && pwd )
      branch = %x( cd #{parent[:repo]} && git rev-parse --abbrev-ref HEAD ).strip
      if branch != 'master'
        fail "**ERROR: not on master**"
      end
      %x( cd #{parent[:repo]} && git pull origin master )
      if $?.exitstatus != 0
        fail "**ERROR: cannot pull master branch**"
      end

    }
end

###### Slow versions of the 'maven_version_xml'

# def maven_version_exec(dir)
#   %x{cd #{dir} && mvn -q -Dexec.executable=echo -Dexec.args='${project.version}' --non-recursive exec:exec -offline}
# end

# def maven_version_help(dir)
#   %x{cd #{dir} && mvn help:evaluate -Dexpression=project.version -q -DforceStdout -offline}
# end