use_synth :piano
notes = [:E4,:Fs4,:B4,:Cs5,:D5,:Fs4,:E4,:Cs5,:B4,:Fs4,:D5,:Cs5]


x = 0 # Start at the first note


live_loop :player do   # Create a loop

  play notes[x] # Play the note at position 0
  sleep 0.15

end