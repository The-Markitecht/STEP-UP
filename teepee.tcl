#
# TeePee preprocessor for C source code.
# 2/2009 by Mark Hubbard.

# in future rename this to StepUp: Simple Tcl-Enhanced Preprocessor Upgrade

# #####################################################################
# procs that may be called from the embedded scripts.

proc get {varname} {
    # return the value of a given variable name, from the global context.

    uplevel #0 set $varname
}

proc out {content} {
    # output the given C code in place of the current script block.
    # calls to this proc are cumulative; additional C code will be written then.
    # a newline is NOT appended.

    append ::new_content $content
}

proc outln {content} {
    # output the given C code in place of the current script block.
    # calls to this proc are cumulative; additional C code will be written then.
    # a newline is appended.

    append ::new_content $content \n
}

proc include {file_name {section_name {}}} {
    # the given file name can be absolute, or relative in which case each path in the ::include_paths list is attempted.
    # a section name may also be passed.  in that case only a section of the file
    # will be included.  the section is between a corresponding marker such as <<-@name->> and the next such
    # marker (of any name) or the end of the file.

    set final {}
    foreach path $::include_paths {
        set full [file join $::source_dir $path $file_name]
        if {[file exists $full]} {
            set final $full
            break
        }
    }
    if {[string equal $final {}]} {
        puts "TeePee: Included file '$file_name' was not found on any path in ::include_paths."
        exit
    }
    puts "TeePee: include $final"
    set inf [open $final r]
    set content [read $inf]
    close $inf

    if {[string length $section_name] > 0} {
        puts "TeePee: include section '$section_name'"
        set re { \< \< - @ \s* }
        append re $section_name
        append re { \s* - \> \> }
        if { ! [regexp -expanded -indices $re $content marker_indices]} {
            puts "TeePee: Included file '$file_name' did not declare a bookmark '$bookmark'."
            exit
        }
        set section_start_pos [expr [lindex $marker_indices 1] + 1]
        set section_end_pos end
        set re { \< \< - @ \s* \w+ \s* - \> \> }
        if {[regexp -expanded -indices -start $section_start_pos $re $content next_indices]} {
            set section_end_pos [expr [lindex $next_indices 0] - 1]
        }        
        set content [string range $content $section_start_pos $section_end_pos]
    }

    out $content
}

# #####################################################################
# procs that are internal to TeePee; these should not be called from the embedded scripts.

proc teepee_process {in_file_name out_file_name} {
    # process the given input file, overwriting the given output file.
    # both should be absolute paths.


    puts "TeePee: writing $out_file_name"
    
    # test TeePee integration with the rest of the tool chain.
    #file copy -force $in_file_name $out_file_name
    #return

    # delete any existing output file in case the processing fails.
    file delete -force $out_file_name

    # read the original source code content
    set inf [open $in_file_name r]
    set content [read $inf]
    close $inf

    # evaluate all blocks of Tcl script enclosed in markers   <<-   ->>
    # the result is inserted in place of the script block.
    set cursor 0 
    set re { (?: \< \< - \s+ (.*?) \s+ - \> \> ){1,1}? }
    while {[regexp -indices -expanded -nocase -start $cursor $re $content ind scriptind]} {
        set match_start [lindex $ind 0]
        set match_end [lindex $ind 1]
        set script_start [lindex $scriptind 0]
        set script_end [lindex $scriptind 1]
        set script [string range $content $script_start $script_end]
        set ::new_content {}
        uplevel #0 $script
        set content [string replace $content $match_start $match_end $::new_content]
        set cursor $match_start
        #incr cursor [string length $::new_content] 
        # don't increment the cursor there.  begin re-scanning at the start of the new content.
        # this catches include files, and any other misc script that might generate additional substitution tokens.
        set ::new_content {}
    }

    # replace all Tcl variables prefixed with $$
    # an optional array index in parenteses is also accepted.
    set cursor 0 
    set re { \$\$ ( [a-z0-9_:]+ (?: \( [a-z0-9_ ]+ \) )? ) }
    while {[regexp -indices -expanded -nocase -start $cursor $re $content ind nameind]} {
        set match_start [lindex $ind 0]
        set match_end [lindex $ind 1]
        set name_start [lindex $nameind 0]
        set name_end [lindex $nameind 1]
        set name [string range $content $name_start $name_end]
        set result [uplevel #0 set $name]
        set content [string replace $content $match_start $match_end $result]
        set cursor $match_start
        #incr cursor [string length $result] 
    }
    
    # write new source code content.
    set outf [open $out_file_name w]
    puts $outf $content
    close $outf
}

proc teepee_main {} {
    uplevel #0 {
        set c_compiler [string map -nocase {\\ /} [lindex $argv 0]]
        set teepee_dir [file dirname [info script]]

        # scan each option to be passed to the C compiler.
        # for each one that's *.c replace it with a *_tp.c filename and a -o output option
        # specifying the *.o filename based on the original *.c filename.
        # all other options pass through as-is, so the linker can also be called through here.
        set c_options [list]
        foreach cla [lreplace $argv 0 0] {
            if {[string match -nocase *.c $cla]} {
                set ::new_content {}
                set cla [file join $teepee_dir $cla]
                set ::source_dir [file dirname $cla]
                
                set ::include_paths [list $::source_dir]
                set outfn [file rootname $cla]_tp.c
                teepee_process $cla $outfn
                file attributes $outfn -readonly 1
                #lappend c_options -o [file rootname [file tail $cla]].o
                lappend c_options $outfn
            } else {
                lappend c_options $cla
            }
        }

        puts "TeePee: $c_compiler $c_options"
        eval exec -keepnewline $c_compiler $c_options >&@stdout
    }
}

# #####################################################################
# main line

# other Tcl script can set teepee_lib_only and then source this file, to 
# gain access to the teepee processor procs as a library.
if { ! [info exists teepee_lib_only]} {
    teepee_main
}


