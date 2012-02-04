
# additional procs for use in TeePee enhanced Verilog source files.

# K:\Microcontrollers\Projects\adc1801test\source>"c:\Program Files\TclPro1.4\win32-ix86\bin\tclsh83.exe" teepee_verilog_lib.tcl
# or add this to the bottom of the project's .qsf TCL file:
# set_global_assignment -name PRE_FLOW_SCRIPT_FILE "quartus_sh:source/teepee_verilog_lib.tcl"

# FSM builder; see notes in source of cycle_accurate_model_test 
# see also K:\Microcontrollers\Projects\adc1801test\source, which demonstrated the need for FSM builder.
# possible product name: FISTcl (FInite State Tcl) pronounced "fist tickle" as one word, emphasis on first syllable.

set long_syntax_brainstorm {
    long syntax is very undesirable compared to short syntax.
    deprecate it.  instead introduce a 3rd syntax which works like short syntax, but allows
    a state to occupy any number of lines until the next state label is encountered.
    that way it doesn't need open & close braces.
    allow anonymous states to use a simple label such as :: (which is empty label) or better yet
    just one colon, so it won't clash with Tcl.
}

set routines_brainstorm {
    to really make subroutines i need an easy way to declare a "return to state" register, which is automatically
    the same width as the state_reg.  maybe a Tcl variable or command that retrieves the width?? 
    yeah cause i'll need it for other uses too.
    could not use it until during or after final Verilog generation, since the width is not known until then.
    or even a real call stack.
    for now i'll hack around this by making a "return to" register of a known width.
    just jumping to a state that's specified by a given register would be good for this.
    to do it just transfer the given register's data into the state register.
    specifying the register name like this also enables mutliple "return to" registers,
    eliminating the need for a real call stack.
    the return-to register width doesn't have to be known.  it could be oversized; always extract a bit-range so Quartus never complains.
    then just provide a way to load a state label into that register.  using the standard load keyword
    would be best; it supports Verilog arithmetic, so i could load the register with "current state + 1" for convenience?
    no, i'd have to know the state reg width for the Verilog numeric constant.
    instead enhance the load keyword.  if the data expression contains any $ then use Tcl expr instead of Verilog.
    make sure $current_state_num always contains the current state number.
    so a typical jump-to-routine looks like this:  :label: assert blah; load return_reg {$current_state_num + 1}; jump ROUTINE;
    and at the end of ROUTINE do this: jump reg return_reg
    better yet: streamline both, and avoid keyword proliferation, like this: 
        :label: assert blah; jump routine ROUTINE;
        jump return;
    "jump routine" syntax can still be used with branching in the same state.
    just branch to wherever, no else, then jump routine.  can invert the branch condition as needed.
    (in future enhance the branch keyword to accept "routine" prefix on its branch destination labels as well.)
    don't bother specifying return register name.  instead compute it from the routine label.  
    then one return register gets auto-declared for each routine that ever gets called.
    lots of routines can be used super-easy, and nested at run time.  the only restriction is no re-entrance or recursion.
    that's a great tradeoff; if i needed that kind of muscle i should use a general purpose processor.
    do all that and test it with the analyzer FSM; compare the analyzer's output with & without the routine support.
}

proc tpv_sm_write_state_machine {sm_name} {
    namespace eval ::fsm::$sm_name {
        # check preconditions.
        if { ! [info exists state_reg_name] } {
            error "State machine $sm_name does not specify a state_reg.  That is required."
        }
        if { ! [info exists clock_signal] } {
            error "State machine $sm_name does not specify a clock.  That is required."
        }
        if { ! [info exists reset_signal] } {
            error "State machine $sm_name does not specify a reset signal.  That is required."
        }
        
        # initialize variables for this proc.
        set num_states [llength $states]
        set max_state [expr $num_states - 1]
        set max_bit 0
        set num_bits 1
        set nstemp 2
        while {$nstemp < $num_states} {
            set nstemp [expr $nstemp * 2]
            incr num_bits
            incr max_bit
        }
        set width "${num_bits}'d"

        # write FSM header code.
        outln "
    // state machine $sm_name holds state in $state_reg_name
    reg\[${max_bit}:0\] $state_reg_name ;
        "
        set routines [lsort -unique $routines]
        foreach routine $routines {
            outln "
    reg\[${max_bit}:0\] ${routine}_return_to_state ;
            "
        }

        # output combinational signals
        foreach output_record $outputs {
            foreach {out_num_bits out_signal_name default_level} $output_record {
                # for this signal, collect a fully defined list of levels in each state.
                set levels [list]
                for {set state_num 0} {$state_num < $num_states} {incr state_num} {
                    set sname [lindex $states $state_num]
                    set avar "state::${sname}::asserts(${out_signal_name})"
                    if {[info exists $avar]} {
                        lappend levels [set $avar]
                    } else {
                        lappend levels $default_level
                    }
                }
                # write Verilog code for "assign" statement.  essentially a multiplexer.
                outln "    assign $out_signal_name ="
                set delim { }
                for {set state_num 0} {$state_num < $num_states} {incr state_num} {
                    set level [lindex $levels $state_num]
                    if {$out_num_bits < 2} {
                        set level [vbool $level lenient]
                    }
                    outln "        $delim $state_reg_name == ${width}$state_num ? ( $level )"
                    set delim {:}
                }
                if {$out_num_bits < 2} {
                    set default_level [vbool $default_level lenient]
                }
                outln "        : ( $default_level ) ;"
            }
        }
        
        outln "
    always @ ( $clock_edge ( $clock_signal )) begin"
            
        # write arbitrary Verilog code to run on every clock edge.
        foreach chunk $body {
            outln "        [string trimleft $chunk]"
        }

        # write Verilog code for each state, including reset state.
        for {set state_num 0} {$state_num < $num_states} {incr state_num} {
            set sname [lindex $states $state_num]

            # format comments for this state.
            set comments {}
            foreach chunk [set state::${sname}::comments] {
                append comments "\n            // $chunk"
            }

            if {$state_num == 0} {
                outln "
        if ((( $reset_signal ) == [vbool $reset_level strict]) || ($state_reg_name == $width$state_num)) begin
            // reset $comments
                "
            } else {
                outln "
        end else if ($state_reg_name == $width$state_num) begin
            // state $sname $comments
                "
            }
            
            # write arbitrary Verilog code for this state.
            foreach chunk [set state::${sname}::body] {
                outln "            [string trimleft $chunk]"
            }

            # write Verilog code for all state transitions, including branches and jumps.
            # first make sure default_jump (if any) is resolved to default_dest_num in all cases.
            set default_dest_name proceed
            set default_dest_modifier {}
            if [info exists state::${sname}::default_jump] {
                set djl [set state::${sname}::default_jump]
                set default_dest_modifier {}
                set default_dest_name [lindex $djl 1]
            }
            set default_dest_num [find_state_num $sm_name $state_num $default_dest_name]
#patch: routine/return keywords broken by reworking for multiple jump destinations; fix it in future.            
#            if {$default_dest_modifier == {routine}} {
#                set default_jump_code "${default_dest_name}_return_to_state <= ${width}[expr {$state_num + 1}];  \
#$state_reg_name <= ${width}$default_dest_num ; // $default_dest_name"
#            } elseif {$default_dest_modifier == {return}} {
# #patch: how do you know which return_to register to use here?
# #guess i'll have to specify it in the syntax.  bummer.
# #try searching the list of all called routines in the FSM.  return to the lowest positioned
# #one that's above the return line.
#                set default_jump_code "$state_reg_name <= ${width}$default_dest_num ; // $default_dest_name"
#            } else {
#                set default_jump_code "$state_reg_name <= ${width}$default_dest_num ; // $default_dest_name"
#            }
#            if [info exists state::${sname}::branches] {
#                # write Verilog code for one or more conditional transitions.
#                foreach branch [set state::${sname}::branches] {
#                    foreach {condition dest_name} $branch {
#                        set dest_state_num [find_state_num $sm_name $state_num $dest_name]
#                        outln "            if ( $condition ) $state_reg_name <= ${width}$dest_state_num ; // $dest_name"
#                    }
#                }
#                #{                }
#                outln "            else begin"
#                outln "                $default_jump_code"
#                outln "            end"                                   
#            } else {
#                # there are no conditional transitions.  write Verilog code for an unconditional one.
#                outln "            $default_jump_code"
#            }
            set jump_code "${width}$default_dest_num /* $default_dest_name */"
            if [info exists state::${sname}::branches] {
                # write Verilog code for one or more conditional transitions.            
                foreach branch [lreverse [set state::${sname}::branches]] {
                    foreach {condition dest_name} $branch {
                        set dest_state_num [find_state_num $sm_name $state_num $dest_name]
                        set jump_code "(($condition) ? ${width}$dest_state_num /* $dest_name */ : $jump_code)"
                    }
                }            
            }
            outln "            $state_reg_name <= $jump_code ; "
        }

        # write FSM footer code.
        outln "
        end else begin
            // unknown state; reset.
            $state_reg_name <= 0;
        end
    end
        "
    }
}

proc write_jump_verilog {sm_name state_reg_name current_state_num dest_modifier dest_name indent} {
    outln "${indent}$state_reg_name <= ${width}$default_dest_num ; // $default_dest_name"
}

proc find_state_num {sm_name current_state_num find_state_name} {
    # searches for and returns the state number for the given find_state_name.
    # can also accept the keyword "stay" meaning the current state.
    # can also accept the keyword "proceed" meaning the next state in order of definition.
    # also the state "reset" (lowercase) always refers to the preemptive state defined by the "reset" specification.

    namespace eval ::fsm::$sm_name {
        set current_state_num [uplevel set current_state_num]
        set find_state_name [uplevel set find_state_name]
        if {$find_state_name == {stay}} {
            return $current_state_num
        } elseif {$find_state_name == {proceed}} {
            if {$current_state_num == $max_state} {
                return 0
            } else {
                return [expr {$state_num + 1}]
            }            
        } else {
            set found_state_num [lsearch -exact $states $find_state_name]
            if { $found_state_num < 0 } {
                error "Unrecognized state $find_state_name in branch or jump specified in FSM $sm_name state [lindex $states $current_state_num]"
            }
            return $found_state_num
        }
    }
}

proc tbool {value strict} {
    # returns a Tcl boolean (int 1 or 0) by converting the given integer or string.
    # strict parsing when strict is string "strict".  error on invalid data then.
    # otherwise returns the same invalid string that was passed.

    if {[string is integer -strict $value]} {
        return [expr {$value != 0}]
    }
    set vl [string tolower $value]
    if {$vl == {true}} {return 1}
    if {$vl == {high}} {return 1}
    if {$vl == {hi}} {return 1}
    if {$vl == {false}} {return 0}
    if {$vl == {low}} {return 0}
    if {$vl == {lo}} {return 0}
    if {$strict == {strict}} {
        error "$value is not a valid boolean or logic level."
    } 
    return $value
}

proc vbool {value strict} {
    # returns a Verilog boolean by converting the given integer or string.
    # strict parsing when strict is string "strict".  error on invalid data then.
    # otherwise returns the same invalid string that was passed.

    set tb [tbool $value $strict]
    if {$tb == $value} {return $value}
    if {$tb == 1} {return {1'b1}}
    return {1'b0}
}

proc tpv_sm_state_reg {sm_name state_reg_name} {
    # name the register where the machine will hold its state.

    namespace eval ::fsm::$sm_name {  
        set state_reg_name [uplevel set state_reg_name]
    }
}


proc tpv_sm_clock {sm_name clock_edge clock_signal} {
    # name the clock signal and edge for timing all state transitions.

    namespace eval ::fsm::$sm_name {  
        set clock_edge [uplevel set clock_edge]
        set clock_signal [uplevel set clock_signal]
    }
}


proc tpv_sm_reset {sm_name reset_signal active_level {state_body {}}} {
    # name the signal for synchronous reset, and describe how to respond to it.
    # hand-written RESET states are no longer used; they are folded into this implementation.
    # reset will always be state 0.
    # test for the reset signal will pre-empt any other transition and jump directly to reset state.
    # allow other states to specify a transition to reset using another special destination name "reset".
    # it should also assert all combinational's to their defaults.  (this is done automatically if 
    # there are no assert's in its state_body.)
    # it should also branch to stay as long as the reset signal is active.
    # the "stay" cycles represent a hold time for all the combinational and sequential outputs before
    # resuming normal operation.
    # reset specification should accept an optional state_body to augment it, e.g. for clearing additional registers,
    # and/or specifying a branch or jump with which to begin normal operation.
    # if there is no branch or jump, the FSM will transition to its first specified state to begin normal operation.
    # (implement all that by simply pre-loading an entry into the conditional branches list before evaluating the Verilog body.)        

    # initialize the state's namespace just like tpv_sm_state does, so it will be ready in time for tpv_sm_branch.
    namespace eval ::fsm::${sm_name}::state::reset {  
        set body {}
    }        
    namespace eval ::fsm::$sm_name {  
        if {[llength $with_signals] > 0} {
            error "State machine $sm_name specifies its reset state inside a 'with' signal block.  That is not allowed."
        }
        set reset_signal [uplevel set reset_signal]
        set reset_level [uplevel set active_level]  
        # pre-load the branches list with a branch that stays in the reset state as long as the reset signal remains active.
        # then create the reset state by parsing code in the same manner as most others.
        tpv_sm_branch $sm_name reset "( $reset_signal ) == [vbool $reset_level strict]" stay
        tpv_sm_state $sm_name reset [uplevel set state_body]
    }
}


proc tpv_sm_output {sm_name comb_word num_bits signal_name default_level} {
    # describe an output signal which the state machine will drive.

    lappend ::fsm::${sm_name}::outputs [list $num_bits $signal_name $default_level]
}


proc tpv_sm_load {sm_name state_name reg_name data_expression} {
    # load the given register with the given Verilog data expression at the END of each clock cycle
    # where the state machine is in the state that is being described.
    # this is typically described inside a state or states block.
    
    namespace eval ::fsm::${sm_name}::state::${state_name} {  
        lappend body "[uplevel set reg_name] <= ( [vbool [uplevel set data_expression] lenient]);"
    }
}

proc tpv_sm_assert {sm_name state_name signal_name data_expression} {
    # assert the given signal with the result of the given Verilog data expression.
    # the data is asserted onto the signal wire only during the state in which the assert is described.
    # it does not remain after the next state transition.
    # this is a combinational operation, not sequential, so it is implemented using Verilog continuous 
    # syntax outside of the always block.
    # this is typically described inside a state or states block.

    namespace eval ::fsm::${sm_name}::state::${state_name} {  
        set asserts([uplevel set signal_name]) [uplevel set data_expression]
    }
}

proc tpv_sm_jump {sm_name state_name modifier {dest {}}} {
    # describe the next state the machine will transition to on the next clock edge.
    # this is an unconditional transition.

    # only one jump (or one branch with an "else" clause) can appear inside a given state description.
    # if there are none, then the machine will proceed to the next state described after
    # this one, on the next clock edge.
    
    # normal syntax for jump is: jump label
    # there are also 2 optional special forms of jump:
    # jump routine label
    # jump return
    # where "jump", "routine" and "return" are literal words, and "label"
    # is the label of the desired state to transition to.
    # these cause an automatic jump-to-subroutine transition
    # and return-from-subroutine transition.  the machine will
    # memorize the next state after the current one, then 
    # go to the given state, "execute" that state and any
    # successive ones as if they formed
    # a subroutine on a general purpose processor.  during that,
    # when it encounters "jump return", it returns back to the 
    # next state after the one that did the "jump routine".
    # this is great for expressing the same sequence of operations
    # in several places in a FSM without the expense of boilerplate code.
    # these routines can be nested with no limit of depth.
    # one routine can call another (and another)
    # and the two (or more) returns will work as expected.
    # the only limitation is that each routine cannot be re-entered
    # while it is already in use.  doing so will cause loss of
    # the first call, and the FSM will return to the wrong state
    # when it executes the return for the first call (the outer call)
    # to that routine.  the return state for the most recent call
    # (the most inner nested call) is the only one that can be 
    # honored correctly.  this is because there is no call stack
    # like a general purpose processor would use.  if you need
    # that much muscle, start using a general purpose processor instead of FSM.
    # timing of jump routine and jump return are the same as
    # regular jumps.  so place them in their own state by themselves
    # only if you actually want them to take up a cycle.
    # these can be used as the default jump in a state with
    # conditional branches.  any of the 3 forms of jump can.
    # just don't specify an "else" for any of the branches in
    # that state.  the "jump" works as if it were the "else".
    
    set vn ::fsm::${sm_name}::state::${state_name}::default_jump
    if [info exists $vn] {
        error "More than one unconditional jump specified in FSM $sm_name state $state_name"
    }
    if {$dest == {}} {
        set $vn [list {} $modifier] ;# no modifier given; this is the destination.
    } else {
        set modifier [string tolower $modifier]
        set $vn [list $modifier $dest]
        if {$modifier == {routine}} {
            lappend ::fsm::${sm_name}::routines $dest
        }
    }
}

proc tpv_sm_branch {sm_name state_name condition dest {else_word {}} {else_dest {}}} {
    # describe a state the machine might transition to on the next clock edge,
    # and a rule to decide whether to do it.
    # the rule will be checked on each clock cycle the machine spends in the current state.
    # this is a conditional transition.
    # an optional "else" clause may also be specified.  if it is specified, the machine will transition to
    # that alternative state on the first clock cycle where the rule was false.

    # only one jump (or one branch with an "else" clause) can appear inside a given state description.
    # if there are none, then the machine will proceed to the next state described after
    # this one, on the next clock edge.
    
    lappend ::fsm::${sm_name}::state::${state_name}::branches [list $condition $dest]
    set else_fully_specified [expr {($else_word == {else}) && ($else_dest != {})}]
    if $else_fully_specified {
        tpv_sm_jump $sm_name $state_name $else_dest
    } elseif {$else_word == {}} {
        # do nothing
    } else {
        error "Syntax error in branch $dest in FSM $sm_name state $state_name"
    }
}

proc tpv_sm_wait {sm_name state_name num_cycles} {
    # remain in the current state for the given total number of cycles.
    # 0 is not a valid number.
    # any transition out of the current state will take 1 cycle, so if the given total is 1,
    # no additional cycles will be spent.  the next clock edge will cause a transition
    # out of the current state.
    # if the given total is greater than 1, a counter will be loaded.  
    # the next clock edge after the counter reaches 0 will cause a transition
    # out of the current state.
    # this instruction cannot appear in the same state as a branch.
    
    namespace eval ::fsm::${sm_name}::state::${state_name} {  
        lappend body "//wait [uplevel set num_cycles]"
#patch: need implementation       
    }
}

#proc tpv_sm_label {sm_name label_name} {
#    # provides a name for the state defined on the next line, when using short syntax.
#    
#    namespace eval ::fsm::${sm_name} {  
#        set next_state_label [uplevel set label_name]
#    }
#}

proc tpv_sm_state_verilog {sm_name state_name verilog_body} {
    # insert a block of arbitrary Verilog code into the current state.

    namespace eval ::fsm::${sm_name}::state::${state_name} {  
        lappend body [uplevel set verilog_body]
    }
}

proc tpv_sm_verilog {sm_name verilog_body} {
    # insert a block of arbitrary Verilog code into the current state.

    namespace eval ::fsm::${sm_name} {  
        lappend body [uplevel set verilog_body]
    }
}

proc tpv_sm_state {sm_name state_name state_body} {
    # describe a state within the current state machine using long block syntax.
    # call this repeatedly to describe back-to-back states.

    namespace eval ::fsm::$sm_name {
        lappend states [uplevel set state_name]
    }
    namespace eval ::fsm::${sm_name}::state::${state_name} {  
        set body {}
        set comments [list]
    }
    
    # implement any "with" signals that are in effect right now.
    foreach {signal level} [set ::fsm::${sm_name}::with_signals] {
        tpv_sm_assert $sm_name $state_name $signal $level
    }
    
    # convert brackets to escaped brackets, so Verilog-style bit slicing can easily happen.
    # also convert double brackets to single brackets, so Tcl nested evaluation can still happen through double brackets.
    #set state_body [string map [list {[[} {[} {[} {\[} {]]} {]} {]} {\]}] $state_body]
    #don't do this; this itself causes trouble when bit slicing is used in e.g. a branch condition.
    #instead use Tcl braces {} for all such syntax, including branch conditions, register loads, etc.
    #it was already used in branch conditions anyway.
    
    interp alias {} load {} tpv_sm_load $sm_name $state_name
    interp alias {} assert {} tpv_sm_assert $sm_name $state_name
    interp alias {} jump {} tpv_sm_jump $sm_name $state_name
    interp alias {} branch {} tpv_sm_branch $sm_name $state_name
    interp alias {} wait {} tpv_sm_wait $sm_name $state_name
    interp alias {} verilog {} tpv_sm_state_verilog $sm_name $state_name
    
    eval $state_body
    
    catch {interp alias {} load {} {} }
    catch {interp alias {} assert {} {} }
    catch {interp alias {} jump {} {} }
    catch {interp alias {} branch {} {} }
    catch {interp alias {} wait {} {} }
    catch {interp alias {} verilog {} {} }
    
    set_sm_context $sm_name ;# required because "verilog" and maybe other instructions are used in both sm and state contexts.
}

set junk {
proc tpv_sm_states {sm_name states_body} {
    # describe a series of states using short syntax of one state per line.
    # these states will be anonymous (nameless) by default, but each can be labeled e.g. so it
    # can be the destination of a branch or jump transition.

    interp alias {} label {} tpv_sm_label $sm_name
    set label_re { ^ \s* label \s+ (\w+) \s* $ }
    foreach state_body [split $states_body "\r\n"] {
        if {[string trim state_body] != {}} {
            if {[regexp -expanded -nocase $label_re $state_body junk label_name]} {
                # this line is a label.
                set next_state_label $label_name
            } else {
                # this line is a state body.  does it have a label pending?
                if {$next_state_label != {}} {
                    set state_name $next_state_label
                    set next_state_label {}
                } else {
                    set state_name "anonymous[llength ::fsm::${sm_name}::states]"
                }
                tpv_sm_state $sm_name $state_name $state_body
            }
        }
    }
    interp alias {} label {} {}
}
}

proc tpv_sm_states {sm_name states_body} {
    # describe a series of states using short syntax of one state per line.
    # these states will be anonymous (nameless) by default, but each can be labeled e.g. so it
    # can be the destination of a branch or jump transition.

    set states_body [string map [list "\r\n" "\n" "\r" "\n"] $states_body]
    
    set label_re { ^ \s* ( \: \s* (\w+) \s* \: )? (\#{1,2})? \s* ( (\w+) ( \s+ .+ )? ) $ }
    #set chars_consumed 0
    set block {}
    set comments [list]
    foreach state_body [split $states_body "\n"] {
        if {$block == {}} {    
            # not building up an enclosed block right now.  process each line.
            if {[string trim $state_body] != {}} {
                #outln "// >>>$state_body<<<"
                set state_name {} ;# re-initialize any variables that are populated by optional parts of the regex; those might not match.
                set code_remainder {}
                set comment_mark {}
                if { ! [regexp -expanded -nocase $label_re $state_body junk junk2 state_name comment_mark code code_first_word code_remainder]} {
                    error "Syntax error in FSM $sm_name, in short-syntax line: [string trim $state_body]"
                }
                if {$comment_mark == {#}} {
                    # comment; not a real state; accumulate this and memorize it along with the next state that follows.
                    # this only sees full-line comments that fall between the intended state and the previous state.
                    # it does not see inline comments at the end of the state body.  
                    # that would require an additional command alias in tpv_sm_state such as ## .
                    lappend comments $code 
                } elseif {[info complete $state_body]} {
                    # this line is recognized as a state body.  does it have a label at the front, or does it need an anonymous name assigned?
                    if {$state_name == {}} {
                        set state_name "anonymous[llength [set ::fsm::${sm_name}::states]]"
                    }
                    #outln "// $state_body"
                    tpv_sm_state $sm_name $state_name $code
                    # now prepend any loose comments to those that were defined within this state.
                    set cvn "::fsm::${sm_name}::state::${state_name}::comments"
                    set $cvn [concat $comments [set $cvn]]
                    set comments [list] ;# reset accumulation.
                } else {
                    # this line has an unmatched Tcl delimiter.  begin building a code block?
                    if { [string tolower $code_first_word] == {with} } {
                        append block $state_body "\n"
                    } else {
                        error "Syntax error in FSM $sm_name.  Possible unmatched delimiter in short-syntax line: [string trim $state_body]"
                    }
                }
            }
            #incr chars_consumed [string length $state_body]
            #incr chars_consumed
        } else {
            # building up a block right now.  accumulate until it's complete, then process it all together.
            append block $state_body "\n"
            if {[info complete $block]} {
                #tpv_sm_code_block $sm_name $block
                interp alias {} with {} tpv_sm_with_block $sm_name
                eval $block
                #interp alias {} with {} {} 
                    # ^^^ don't cancel this alias; that's causing errors in nested with blocks.
                set block {}
            }
        }
    }
    if {$block != {}} {
        error "Syntax error in FSM $sm_name.  Possible unmatched delimiter in short-syntax block."
    }

}

proc tpv_sm_with_block {sm_name output_signal_name output_level code_block} {
    # assume that the given output signal is "assert"ed to the given level in every state
    # defined in the given code block.
    # "with" blocks can be nested too.
    # in any case, explicit "assert"s within a given state override those given by "with" if they
    # have the same signal name.
    
    namespace eval ::fsm::$sm_name {
        lappend with_signals [uplevel set output_signal_name] [uplevel set output_level]        
    }

    tpv_sm_states $sm_name $code_block

    namespace eval ::fsm::$sm_name {
        if {[lindex $with_signals end-1] != [uplevel set output_signal_name]} {
            error "Syntax error in FSM $sm_name.  Possible unmatched delimiter in short-syntax \"with\" block."
        }        
        set with_signals [lrange $with_signals 0 end-2]
    }
}

set junk {
proc tpv_sm_code_block {sm_name block} {
    # process the given multi-line block of code, which includes opening & closing curly braces.
    # all line breaks are assumed to be already normalized to \n .
    
    set code_first_word [lindex $block 0]
    if { $code_first_word == {with} } {
    }
}
}

proc set_sm_context {sm_name} {
    interp alias {} state_reg {} tpv_sm_state_reg $sm_name
    interp alias {} clock {} tpv_sm_clock $sm_name
    interp alias {} reset {} tpv_sm_reset $sm_name
    interp alias {} output {} tpv_sm_output $sm_name
    interp alias {} verilog {} tpv_sm_verilog $sm_name
    interp alias {} state {} tpv_sm_state $sm_name
    interp alias {} states {} tpv_sm_states $sm_name
}

proc describe_state_machine {syntax_version_number sm_name sm_body} {
    # completely describe a finite state machine using Tcl-like syntax.

    if {[string tolower $syntax_version_number] != {v1}} {
        error "State machine $sm_name specifies syntax version $syntax_version_number which is not supported.  Only v1 is supported."
    }
    
    catch {namespace delete ::fsm::$sm_name}
    namespace eval ::fsm::$sm_name {
        set sm_name [uplevel set sm_name]
        set states [list] ;# defines order of states.
        set next_state_label {}
        set outputs [list]
        set body [list]
        set with_signals [list]
        set routines [list] ;# this may have duplicates in it.
    }
    
    set_sm_context $sm_name
    
    eval $sm_body
    
    catch {interp alias {} state_reg {} {} }
    catch {interp alias {} clock {} {} }
    catch {interp alias {} reset {} {} }
    catch {interp alias {} output {} {} }
    catch {interp alias {} verilog {} {} }
    catch {interp alias {} state {} {} }
    catch {interp alias {} states {} {} }
    
    tpv_sm_write_state_machine $sm_name
}

# ##################################################################################
# TEST CASES
# ##################################################################################

set test_cases {

#patch: testing hack:
set ::src_path [file dirname [info script]]
set ::out_file [open [file join $::src_path fsm.v] w]
proc outln {src} {
    puts $::out_file $src
}
outln {
module FSMs (
    input clk50m,
    input rst,
    input capture_clk
);
}        

# example of state machine description for alternating up/down counter.
outln {
reg[7:0] addr;
}
describe_state_machine v1 updowncnt {
    state_reg updowncnt_state
    clock posedge clk50m
    output comb 1 write_en lo
    reset rst hi {
        load addr 8'd0
    }    
    state COUNTUP {
        load addr {addr + 8'd1}
        assert write_en hi
        branch {addr == 8'hFE} proceed else stay
    }
    state COUNTDOWN {
        verilog {
            if (addr == 8'd20)
                addr <= 8'd15;
            else
                addr <= addr - 8'd1;
        }
        assert write_en { addr[1:0] == 2'b11 }
        branch {addr == 8'h01} proceed else stay
    }    
    state COOLOFF {
        wait 5
        jump COUNTUP 
    }
}


# example of state machine description for sequential controller.
# shows alternate syntax for compact series of anonymous states.
outln {
wire[7:0] ram_output; // assigned to data output of RAM block.
reg[7:0] buffer;
assign ram_input = buffer;
wire[3:0] addr4;
}
describe_state_machine v1 sctrl {
    state_reg sctrl_state
    clock posedge clk50m
    output comb 1 write_en2 lo
    output comb 1 read_en lo
    output comb 4 addr4 4'd0
    output comb 1 led hi    
    reset rst hi    
    states {
        assert addr4 4'h3; assert read_en hi;
        load buffer ram_output
        :WRITE_AGAIN: assert addr4 4'h4; assert write_en2 hi;
        assert addr4 4'h5; assert write_en2 hi; assert led lo; wait 2;
        assert addr4 4'h6; assert write_en2 hi;
        assert addr4 4'h7; assert write_en2 hi; jump WRITE_AGAIN;        
    }
}

# example of state machine description for sequential controller.
# shows branching to alternate states based on inputs.
outln {
wire[7:0] ram_output2; // assigned to data output of RAM block.
reg[15:0] total;
reg[7:0] addr2;
}
describe_state_machine v1 branchdemo {
    state_reg branchdemo_state
    clock posedge clk50m    
    reset rst hi {
        load total 16'd0 
        load addr2 8'd0
    }
    states {
        :AGAIN: load addr2 {addr2 + 8'd1}
        wait 1 
        branch {ram_output2 == 8'h5a} AGAIN else proceed
        load total {total + ram_output2}; jump AGAIN;
    }
}

# example of state machine description for logic analyzer.
outln {
wire[31:0] ram_data;
reg[8:0] addr3;  
reg[7:0] tx_data;
reg[1:0] tx_busy_sync;
wire tx_busy;
}
describe_state_machine v1 analyzer {
    state_reg analyzer_state
    clock posedge capture_clk
    output comb 1 ready lo
    output comb 1 capturing lo
    output comb 1 dumping lo
    output comb 1 tx_load lo
    
    verilog {
        // synchronizer for tx_busy.
        tx_busy_sync[0] <= tx_busy;
        tx_busy_sync[1] <= tx_busy_sync[0];
    }
        
    reset rst hi {
        load addr3 9'd0
    }
    
    states {
        :READY: assert ready hi; branch {capture_trigger} proceed else stay;
        :CAPTURING: assert capturing hi; load addr3 {addr3 + 9'd1}; branch {addr3 == 9'b111111111} proceed else stay;
        with dumping hi {
            :DUMP_NEXT_WORD: load tx_data {ram_data[7:0]}
            assert tx_load hi; branch {tx_busy_sync[1]} proceed else stay;
            branch {tx_busy_sync[1]} stay else proceed

            load tx_data {ram_data[15:8]};
            assert tx_load hi; branch {tx_busy_sync[1]} proceed else stay;
            branch {tx_busy_sync[1]} stay else proceed

            load tx_data {ram_data[23:16]};
            assert tx_load hi; branch {tx_busy_sync[1]} proceed else stay;
            branch {tx_busy_sync[1]} stay else proceed

            load tx_data {ram_data[31:24]};
            assert tx_load hi; branch {tx_busy_sync[1]} proceed else stay;
            branch {tx_busy_sync[1]} stay else proceed
            
            load addr3 {addr3 + 9'd1}; branch {addr3 == 9'b111111111} READY else DUMP_NEXT_WORD;
        }
    }
}

outln {endmodule}
close $::out_file
exit
#patch: testing hack
}