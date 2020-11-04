projects = [
  'eclipse/lyo',

  'eclipse/lyo.core',
  'eclipse/lyo.client',
  'eclipse/lyo.server',
  'eclipse/lyo.designer',

  'eclipse/lyo.store',
  'eclipse/lyo.trs-client',
  'eclipse/lyo.trs-server',
  'eclipse/lyo.validation',
  'eclipse/lyo.oslc-ui',

  'eclipse/lyo.docs',
  'eclipse/lyo.rio',

  'eclipse/lyo.domains',
  'eclipse/lyo.ldp',
  'eclipse/lyo.testsuite',
]

if not ENV.has_key?('GITHUB_ACCESS_TOKEN')
  puts 'You shall set \'GITHUB_ACCESS_TOKEN\' env var before running this script'
  exit
end

projects.each { |p|
  # '-A' keeps unknown labels
  # '-d' makes a dry run
  res = %x( github-label-sync -l labels.json -A -d #{p} )
  puts res
  puts "="*79
}