#{*****************************************************************************}
#{                   _                                                         }
#{       /\/\   ___ | | ____ _ _ __ _ __ __ _                                  }
#{      /    \ / _ \| |/ / _` | '__| '__/ _` |                                 }
#{     / /\/\ \ (_) |   < (_| | |  | | | (_| |                                 }
#{     \/    \/\___/|_|\_\__,_|_|  |_|  \__,_|                                 }
#{                                                                             }
#{ Mokarra                                                                     }
#{   (C)opyright 2011-2015 - LeafScale Systems, Inc.                           }
#{                                                                             }
#{ This software is protected under the LeafScale Software License             }
#{                                                                             }
#{*****************************************************************************}
#{ /Rakefile                                                                   }
#{ Mokarra Rakefile - Uses ruby rake to compile and assemble the software.     }
#{*****************************************************************************}


# // SETUP \\
# ----------------------------------------------------------------------------

#//Build Dir - Where the build files (units, libs, binaries) will be stored.
build = "build"


# //Set default language mode for compatability [objfpc,delphi,fpc,tp]
fpc_mode="objfpc"

#//Set Platform Build Target 
fpc_target="darwin"

#//Set custom FPC option flags
#
# -Sm      = Support macros like C (global)
# -Sh      = Use ansistrings by default instead of shortstrings
# -O2      = Set Level 2 Optimization flag
fpc_options="-Sm -O2 -Sh"

#//Set the compiler command line (can be overriden in source per file)
fpc="fpc -M#{fpc_mode} #{fpc_options} -T#{fpc_target}"


# // TASKS \\
# ----------------------------------------------------------------------------

desc "Default compile task 'build'"
task :default => ['contrib','lib_io','lib_system','lib_config','lib_reef','lib_menus','mokarra']
sh "mkdir -p build"

desc "Contributed Libraries"
task :contrib => []


# IO Library
desc "Compiling IO Library..."
task :lib_io do
puts "\nStarting task 'lib_io'..."
sh "mkdir -p build"
basedir="lib/io"
filelist = ['ansicrt','textutils','inputquestions','menu']
  filelist.each do |file|
    puts "Compiling #{basedir}/#{file} => #{build}"
    sh "#{fpc} -o#{build}/#{file} #{basedir}/#{file}"
  end
end

# Config Library
desc "Compiling Config Library..."
task :lib_config do
puts "\nStarting task 'lib_config'..."
sh "mkdir -p build"
basedir="lib/config"
filelist = ['configfile']
  filelist.each do |file|
    puts "Compiling #{basedir}/#{file} => #{build}"
    sh "#{fpc} -o#{build}/#{file} #{basedir}/#{file}"
  end
end

# System Library
desc "Compiling System Library..."
task :lib_system do
  puts "\nStarting task 'lib_system'..."
  sh "mkdir -p build"
  basedir="lib/system"
  filelist = ['version', 'errormsg','systeminfo', 'filesystem']
    filelist.each do |file|
      puts "\nCompiling #{basedir}/#{file} => #{build}"
      sh "#{fpc} -o#{build}/#{file} #{basedir}/#{file}"
    end
end

# Reef Library
desc "Compiling Reef Library..."
task :lib_reef do
  puts "\nStarting task 'lib_reef'..."
  sh "mkdir -p build"
  basedir="lib/reef"
  filelist = ['pkgsrc','reefindex','reefrepo']
    filelist.each do |file|
      puts "\nCompiling #{basedir}/#{file} => #{build}"
      sh "#{fpc} -o#{build}/#{file} #{basedir}/#{file}"
    end
end

# Menus Library
desc "Compiling Menus Library..."
task :lib_menus do
puts "\nStarting task 'lib_menus'..."
sh "mkdir -p build"
basedir="lib/menus"
filelist = ['configmenu']
  filelist.each do |file|
    puts "Compiling #{basedir}/#{file} => #{build}"
    sh "#{fpc} -o#{build}/#{file} #{basedir}/#{file}"
  end
end


# Build EXE
  desc "Linking Binary 'mokarra'..."
task :mokarra do
 puts "\nStarting task 'mokarra'..."
  sh "mkdir -p build"
  puts "\nCompiling and linking executable 'mokarra'"
  sh "#{fpc} -o#{build}/mokarra -emokarra mokarra.lpr"
  basedir="."
  sh "cp #{build}/mokarra #{basedir}"
end

# Clean
desc "Cleaning for Mokarra"
task :clean do
  puts "\nStarting task 'clean'..."
  sh "rm -rf build; rm mokarra"
end

# Remove Backups
desc "Remove Backup Files"
task :rmbackups do
  puts "\nStarting task 'rmbackups'..."
  puts "-- task not written --"
end

