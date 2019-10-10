# Set the default timeout for responses (in seconds)
timeout 0.5

# Define an external channel
external 'door'

# Define a process describing the behavior of the coffee machine.
process('smartdoor') {
  channel('door') {
    stimulus 'lock',  {'passcode' => :integer}
    stimulus 'unlock', {'passcode' => :integer}
    stimuli 'open', 'close'
    responses 'opened', 'closed', 'locked', 'unlocked', 'invalid_command', 'invalid_passcode', 'incorrect_passcode', 'shut_off'
  }
  
  # set initial state
  var 'code', :integer, 0
  var 'subseq_fail', :integer, 0
  goto 'closed_unlocked'
  
  state 'open'
    choice {
      # BEHAVR-02
      o { receive 'close';  send 'closed';  goto 'closed_unlocked' }
      
      # BEHAVR-05
      o { receive 'open';                                       send 'invalid_command'; goto 'open' }
      o { receive 'lock',   constraint: 'passcode == passcode'; send 'invalid_command'; goto 'open' }
      o { receive 'unlock', constraint: 'passcode == passcode'; send 'invalid_command'; goto 'open' }
    }
  
  state 'closed_unlocked'
    choice {
      # BEHAVR-01
      o { receive 'open';   send 'opened';goto 'open' }
      
      # BEHAVR-03
      o { receive 'lock', constraint: 'passcode >= 0 && passcode <= 9999', update:'code = passcode';  send 'locked';  goto 'closed_locked' }
      
      # set invalid code
      o { receive 'lock', constraint: 'passcode < 0 || passcode > 9999';   send 'invalid_passcode'; goto 'closed_unlocked' }
      
      # BEHAVR-05
      o { receive 'close';  send 'invalid_command';   goto 'closed_unlocked' }
      o { receive 'unlock', constraint: 'passcode == passcode'; send 'invalid_command';   goto 'closed_unlocked' }
    }
  
  state 'closed_locked'
    choice {
      # BEHAVR-04
      o {
        receive 'unlock', 
          constraint: 'passcode >= 0 && passcode <= 9999 && passcode==code && subseq_fail!=2', 
          update:'subseq_fail=0', 
          note: '#green subseq_fail reset: $subseq_fail'; 
        send 'unlocked';         
        goto 'closed_unlocked'
      }
      
      # 3 subseq wrong
      o { 
        receive 'unlock', 
          constraint: 'passcode >= 0 && passcode <= 9999 && passcode!=code && subseq_fail==2', 
          note: '#blue lockdown num: $subseq_fail'; 
        send 'incorrect_passcode';
        goto 'lockdown'
      }      
      
      # wrong passcode
      o { 
        receive 'unlock',
          constraint: 'passcode >= 0 && passcode <= 9999 && passcode!=code && subseq_fail!=2',
          update:'subseq_fail=subseq_fail+1',
          note: '#red inc subseq_fail: $subseq_fail';
        send 'incorrect_passcode';
        goto 'closed_locked'
      }
      
      # invalid passcode
      o { receive 'unlock', constraint: 'passcode < 0 || passcode > 9999';send 'invalid_passcode'; goto 'closed_locked' }
      
      # BEHAVR-05
      o { receive 'close';  send 'invalid_command';   goto 'closed_locked' }
      o { receive 'open';   send 'invalid_command';   goto 'closed_locked' }
      o { receive 'lock', constraint: 'passcode >= 0';   send 'invalid_command';   goto 'closed_locked' }
    }
  
  state 'lockdown'    
    
}

