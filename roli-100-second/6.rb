use_synth :piano
notes = [:E4,:Fs4,:B4,:Cs5,:D5,:Fs4,:E4,:Cs5,:B4,:Fs4,:D5,:Cs5]

x = 0
live_loop :player do
  play notes[x]
  x = x + 1

  if x == 12  # If we have reached the end
    x = 0     # go back to the start
  end

  sleep 0.15
end