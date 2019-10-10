# Set the default timeout for responses (in seconds)
timeout 1.0

# Define an external channel
external 'controller'

# Define a process describing the behavior of the coffee machine.
process('coffee machine') {

  # Define stimuli (input) and responses (output) the process can perform on
  # this channel.
  channel('controller') {
    stimuli 'button_coffee', 'button_tea', 'button_lemonade'
    responses 'coffee', 'tea', 'lemonade'
  }

  # Describe the behavior of this process. Statements are read top to bottom,
  # similar to an imperative program.

  state 'start'
    choice{
      o {receive 'button_coffee'; goto 'coffee'}
      o {receive 'button_tea'; goto 'tea'}
      o {receive 'button_lemonade'; goto 'lemonade'}
    }
 
  # coffee
  state 'coffee'
  send 'coffee'
  goto 'start'
  
  # tea
  state 'tea'
  send 'tea'
  goto 'start'
  
  # lemonade
  state 'lemonade'
  send 'lemonade'
  goto 'start'
}

