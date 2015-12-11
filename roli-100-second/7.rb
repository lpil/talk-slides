use_synth :piano
notes = [:E4,:Fs4,:B4,:Cs5,:D5,:Fs4,:E4,:Cs5,:B4,:Fs4,:D5,:Cs5]

x = 0
live_loop :player1 do
  play notes[x]
  x = x + 1
  x = 0 if x == 12
  sleep 0.15
end

y = 0                    # Copy and paste the loop again
live_loop :player2 do
  play notes[y]
  y = y + 1
  y = 0 if y == 12
  sleep 0.15 * 1.02      # The second player is 2% slower
end

# Piano Phase, by Steve Reich