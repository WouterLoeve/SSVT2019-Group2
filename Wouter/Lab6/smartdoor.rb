# Set the default timeout for responses (in seconds)
timeout 0.5

# Define an external channel
external 'door'

# Define a process describing the behavior of the coffee machine.
process('door') {

# Define stimuli (input) and responses (output) the process can perform on
# this channel.
channel('door') {
  stimulus 'open'
  stimulus 'close'
  stimulus 'lock', { 'passcode' => :integer}
  stimulus 'unlock', { 'passcode' => :integer}

  response 'opened'
  response 'closed'
  response 'locked'
  response 'unlocked'
  response 'invalid_command'
  response 'invalid_passcode'
  response 'incorrect_passcode'
}
  
var 'saved_passcode', :integer, 1337
var 'tries', :integer, 0

state 'closed_unlocked'
  choice {
    o {
        receive 'open'
        send 'opened'
        goto 'opened'
      }
    o {
        receive 'close'
        send 'invalid_command'
        goto 'closed_unlocked'
      }
    o {
        receive 'unlock', 
            constraint: 'passcode == passcode' 
        send 'invalid_command'
        goto 'closed_unlocked'
      }
    o {
        receive 'lock', 
            constraint: 'passcode < 0000 || 
                         passcode > 9999'
        send 'invalid_passcode'
        goto 'closed_unlocked'
      }
    o {
        receive 'lock', 
            constraint: 'passcode >= 0000 && 
                         passcode <= 9999', 
            update: 'saved_passcode = passcode'
        send 'locked'
        goto 'closed_locked'
      }
  }

state 'closed_locked'
  choice {
    o {
        receive 'open'
        send 'invalid_command'
        goto 'closed_locked'
      }
    o {
        receive 'close'
        send 'invalid_command'
        goto 'closed_locked'
      }
    o {
        receive 'unlock', 
          constraint: 'saved_passcode == passcode &&
                       passcode >= 0000 &&
                       passcode <= 9999 &&
                       tries != 2', 
          update: 'tries = 0'
        send 'unlocked'
        goto 'closed_unlocked'
      }
    o {
       receive 'unlock', 
          constraint: 'saved_passcode != passcode && 
                       tries == 2 && 
                       passcode >= 0000 && 
                       passcode <= 9999'
       send 'incorrect_passcode'
       goto 'lockdown'
      }
    o {
        receive 'unlock', 
          constraint: 'saved_passcode != passcode &&  
                       passcode >= 0000 && 
                       passcode <= 9999 &&
                       tries != 2',
          update: 'tries = tries + 1'
        send 'incorrect_passcode'
        goto 'closed_locked'
      }
    o {
        receive 'unlock', 
          constraint: 'saved_passcode != passcode &&
                       (passcode < 0000 || 
                        passcode > 9999
                       ) &&
                       tries < 3'
         send 'invalid_passcode'
         goto 'closed_locked'
      }
    o {
        receive 'lock', 
              constraint: 'passcode == passcode'
        send 'invalid_command'; 
        goto 'closed_locked'
      }
  }

state 'opened'
  choice {
    o {
        receive 'open'
        send 'invalid_command'
        goto 'opened'
      }
    o {
        receive 'close'
        send 'closed'
        goto 'closed_unlocked'
      }
    o {
        receive 'unlock', 
          constraint: 'passcode == passcode' 
        send 'invalid_command'
        goto 'opened'
      }
    o {
        receive 'lock', 
          constraint: 'passcode == passcode'
        send 'invalid_command'; goto 'opened'
      }
  }

state 'lockdown'
  
}
