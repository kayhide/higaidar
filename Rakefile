require 'erb'
require 'json'
require 'thor/shell'
require 'pry'

$stage = ENV.fetch('STAGE', 'dev')
raise RuntimeError.new("Invalid STAGE: #{$stage}") unless %w(test dev prod).include?($stage)

$root_dir = __dir__
$dist_dir = "frontend/dist/#{$stage}-deploy"
$config_dir = 'config'
$wrk_dir = '.rake'
$env_file = ".env.#{$stage}.yml"

$app_basename = 'higaidar'
$app_name = "#{$app_basename}-#{$stage}"
$profile = "#{$app_name}-deploy"
$domain_prefix = [$app_basename, ($stage == 'prod') ? nil : $stage].compact.join('-')
$domain = "#{$domain_prefix}.agrishot.com"
$bucket_name = $domain
$api_name = "#{$stage}-#{$app_basename}"
$database_name = $app_name

Dir.chdir $root_dir

def aws cmd
  bin = `which -a aws`.split.reject { |f| f.start_with? '.' }.first
  unless bin
    raise RuntimeError.new 'command not found: aws'
  end
  # puts "#{bin} --profile #{$profile} #{cmd}"
  res = `#{bin} --profile #{$profile} #{cmd}`
  unless $?.success?
    raise RuntimeError.new cmd
  end

  res
end

def env
  @env ||= YAML.load_file($env_file)
end

def say_status *args
  @shell ||= Thor::Base.shell.new
  @shell.say_status *args
end

def with_file file, delete_on_fail: false
  org = File.exists?(file) ? open(file, &:read) : nil
  cur = nil
  updated = false
  begin
    cur = yield
    lift =
      if File.extname(file) == '.json'
        ->(x) { x && JSON.load(x) }
      else
        ->(x) { x }
      end
    if lift.(org) == lift.(cur)
      FileUtils.touch file
    else
      open(file, 'w') { |io| io << cur }
      updated = true
    end
  rescue => err
    if delete_on_fail
      File.delete file if org
      cur = nil
    else
      raise err
    end
  end
  if org && cur
    if updated
      say_status :update, file, :blue
    else
      say_status :identical, file, :yellow
    end
  elsif org
    say_status :remove, file, :magenta
  elsif cur
    say_status :create, file, :green
  end
end


desc 'Display info'
task :info do
  puts "Stage: #{$stage}"
  puts "App name: #{$app_name}"
  puts "Profile: #{$profile}"
  puts "Root dir: #{$root_dir}"
  puts "Dist dir: #{$dist_dir}"
  puts "Domain: #{$domain}"

  endpoint = "#{$wrk_dir}/s3/#{$bucket_name}/endpoint.txt"
  puts "Endpoint: " + (File.exists?(endpoint) && File.read(endpoint) || '')
end

desc 'Open browser'
task :browse do
  dst_dir = File.join($wrk_dir, 's3', $bucket_name)
  endpoint = File.join(dst_dir, 'endpoint.txt')
  unless File.exists? endpoint
    raise RuntimeError.new "#{endpoint} is not found"
  end
  url = File.read(endpoint).sub(/\.s3-website.*amazonaws.com$/, '')
  sh "open #{url}"
end

desc 'Retrieve infomations from AWS'
task :pull => ['s3:pull']

namespace :s3 do
  dst_dir = File.join($wrk_dir, 's3', $bucket_name)
  endpoint = File.join(dst_dir, 'endpoint.txt')
  bucket_uri = "s3://#{$bucket_name}"

  directory dst_dir

  file endpoint => dst_dir do
    aws ['s3', 'mb',  bucket_uri].join(' ')
    region = aws ['configure', 'get', 'region'].join(' ')
    region.chomp!
    open(endpoint, 'w') { |io| io << "http://#{$bucket_name}.s3-website-#{region}.amazonaws.com" }
  end

  desc 'Create S3 bucket'
  task :create => [endpoint] do
    aws ['s3', 'website',
         '--index-document', 'index.html',
         '--error-document', 'error.html',
         bucket_uri].join(' ')
  end

  desc 'Deploy public web to S3 bucket'
  task :deploy => [$dist_dir, endpoint] do
    say_status :deploy, bucket_uri, :green
    aws ['s3', 'sync',
         "#{$dist_dir}/", bucket_uri,
         '--acl', 'public-read',
         '--delete'
        ].join(' ')
  end

  task :pull do
    with_file endpoint, delete_on_fail: true do
      res = aws ['s3api', 'get-bucket-location',
                 '--bucket', $bucket_name].join(' ')
      info = JSON.load(res)
      region = info['LocationConstraint']
      "http://#{$bucket_name}.s3-website-#{region}.amazonaws.com"
    end
  end
end

namespace :apigateway do
  dst_dir = File.join($wrk_dir, 'apigateway', $api_name)
  rest_apis_info = File.join(dst_dir, 'rest_apis_info.json')
  vars = File.join(dst_dir, 'vars.yml')

  directory dst_dir

  task :pull => [dst_dir, vars]

  file rest_apis_info do
    with_file rest_apis_info, delete_on_fail: true do
      aws ['apigateway', 'get-rest-apis'].join(' ')
    end
  end

  file vars => [rest_apis_info] do
    info = JSON.load File.open(rest_apis_info)
    item = info['items'].find { |item| item['name'] == $api_name }
    region = aws ['configure', 'get', 'region'].join(' ')
    region.chomp!

    with_file vars do
      {
        ENDPOINT: "https://#{item['id']}.execute-api.#{region}.amazonaws.com/#{$stage}"
      }.stringify_keys.to_yaml
    end
  end
end


namespace :rds do
  dst_dir = File.join($wrk_dir, 'rds', $database_name)
  db_info = File.join(dst_dir, 'db_info.json')
  my_cnf = File.join(dst_dir, 'my.cnf')
  vars = File.join(dst_dir, 'vars.yml')
  invalidater = File.join(dst_dir, 'invalidater')

  directory dst_dir

  desc 'Create DB instance'
  task :create do
    aws ['rds', 'create-db-instance',
         '--db-instance-identifier', $database_name,
         '--db-instance-class', 'db.t2.micro',
         '--engine', 'mysql',
         '--engine-version', '5.7.16',
         '--allocated-storage', '5',
         '--master-username', env['AWS_RDS_MASTER_USERNAME'],
         '--master-user-password', env['AWS_RDS_MASTER_USER_PASSWORD'],
         '--enable-iam-database-authentication',
         '--publicly-accessible'].join(' ')
  end

  desc 'Show DB instance info'
  task :info => [:invalidate, db_info] do
    puts open(db_info, &:read)
  end

  desc 'Stop DB instance'
  task :stop => [db_info] do
    info = JSON.load File.open(db_info)
    status = info['DBInstances'][0]['DBInstanceStatus']
    raise "DB instance status is not available but: #{status}" if status != 'available'

    aws ['rds', 'stop-db-instance',
         '--db-instance-identifier', $database_name].join(' ')
  end

  task :pull => [:invalidate, dst_dir, db_info, my_cnf, vars]

  task :invalidate do
    FileUtils.touch invalidater
  end

  file invalidater do
    FileUtils.touch invalidater
  end

  file db_info => [invalidater] do
    with_file db_info, delete_on_fail: true do
      aws ['rds', 'describe-db-instances',
           '--db-instance-identifier', $database_name].join(' ')
    end
  end

  file my_cnf => [db_info] do
    info = JSON.load File.open(db_info)
    host = info['DBInstances'][0]['Endpoint']['Address']
    port = info['DBInstances'][0]['Endpoint']['Port']
    user = env['AWS_RDS_MASTER_USERNAME']
    password = env['AWS_RDS_MASTER_USER_PASSWORD']
    with_file my_cnf do
      ['[client]',
       "host=#{host}",
       "port=#{port}",
       "user=#{user}",
       "password=#{password}",
       ''
      ].join("\n")
    end
  end

  file vars => [db_info] do
    info = JSON.load File.open(db_info)
    with_file vars do
      {
        HOST: info['DBInstances'][0]['Endpoint']['Address'],
        PORT: info['DBInstances'][0]['Endpoint']['Port'],
        DBI_RESOURCE_ID: info['DBInstances'][0]['DbiResourceId'],
        USERNAME: 'lambda',
        DATABASE: $app_basename
      }.stringify_keys.to_yaml
    end
  end

  desc 'Start DB console'
  task :cli => [my_cnf] do
    exec 'mysql', "--defaults-extra-file=#{my_cnf}"
  end

  desc 'Create user on DB'
  task :create_user => [my_cnf] do
    cmd = ['mysql',
           "--defaults-extra-file=#{my_cnf}",
           '-e',
           %("CREATE USER 'lambda'@'%' IDENTIFIED WITH AWSAuthenticationPlugin as 'RDS';")
          ].join(' ')
    res = `#{cmd}`
    puts res
    cmd = ['mysql',
           "--defaults-extra-file=#{my_cnf}",
           '-e',
           %("GRANT SELECT,INSERT,UPDATE,DELETE ON *.* to 'lambda'@'%' REQUIRE SSL;")
          ].join(' ')
    res = `#{cmd}`
    puts res
  end
end


ENV['RAILS_ENV'] = $stage
require 'standalone_migrations'

if ENV['ON_REMOTE']
  dst_dir = File.join($wrk_dir, 'rds', $database_name)
  db_info = File.join(dst_dir, 'db_info.json')
  info = JSON.load File.open(db_info)
  StandaloneMigrations::Configurator.environments_config do |stage|
    stage.on $stage do |conf|
      conf.slice('adapter', 'encoding').merge(
        host: info['DBInstances'][0]['Endpoint']['Address'],
        port: info['DBInstances'][0]['Endpoint']['Port'],
        username: env['AWS_RDS_MASTER_USERNAME'],
        password: env['AWS_RDS_MASTER_USER_PASSWORD'],
        database: $app_basename
      ).stringify_keys
    end
  end
end

StandaloneMigrations::Tasks.load_tasks


task :pry => [:environment] do
  class User < ActiveRecord::Base; end
  class Pest < ActiveRecord::Base; end
  binding.pry
end
