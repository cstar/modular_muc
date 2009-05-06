task :default => :build
NAME = "modular_muc"
MVERSION = "0.0.1"

if File.file?('erlang_config.rb') 
  require 'erlang_config'  
else
  puts "erlang_config.rb file is missing."
  puts "You need to fill it with your local configuration."
  puts "erlang default install is /usr/local."
  puts "ejabberd default install is /opt/ejabberd."
  File.open("erlang_config.rb",'w') do |file|
    file.write("ERL_TOP=\"/usr/local\"\n")
    file.write("EJABBERD_TOP=\"/opt/ejabberd\"\n")
  end
  exit(-1)
end
directory "ebin"
directory "doc"
desc "Compile"
task :build => "ebin" do
  sh "#{ERL_TOP}/bin/erl -make"
end

desc "Copies beam files to ejabberd install in #{EJABBERD_TOP}"
task :install => :build do
  sh "cp ebin/*.beam #{EJABBERD_TOP}/lib/ejabberd/ebin"
end

desc "Generate documentation"
task :edoc => "doc" do 
  sh "cp overview.edoc doc/"
  # yes the following line is ugly, but edoc is apparently choking on defines (?)
  source = "[\""+(FileList["src/*.erl"]  - ["src/muc_room_default.erl"]).join("\",\"")+"\"]"
  sh "erl -noshell -eval 'edoc:files(#{source}, [{dir, \"doc\"}])' -s init stop"
end 

task :clean do
  sh "rm -Rf doc/ ebin/"
end

def uname
  @uname ||= `uname`
end 
def erlang_home
    @erlang_home||=IO.popen("#{ERL_TOP}/bin/erl -noinput -noshell -eval 'io:format(code:root_dir()).' -s init stop").readlines[0] 
end