# High level overview:
#
# When a file is saved ->
#   if the file is an Elixir file ->
#     recompile the project
#     run the "test" mix task
#
#
# Nice and simple.
#
# Watching file system events to detect when a file has been changed is the
# tricky part. Luckily someone has already done this for us, and released the
# result on hex as `fs`.

# The fs application allows a process to subscribe to file system events. Each
# time a file is modified it will send subscribers a message containing
# information about the event.

Application.ensure_started(:fs)
:fs.subscribe()

# ...save a file here...

flush()
#
# {#PID<0.119.0>, {:fs, :file_event},
#  {'/home/louis/projects/talks/mix-test-watch/4913', [:undefined]}}
# {#PID<0.119.0>, {:fs, :file_event},
#  {'/home/louis/projects/talks/mix-test-watch/4913', [:modified, :closed]}}
# {#PID<0.119.0>, {:fs, :file_event},
#  {'/home/louis/projects/talks/mix-test-watch/02.exs~', [:renamed]}}
# {#PID<0.119.0>, {:fs, :file_event},
#  {'/home/louis/projects/talks/mix-test-watch/02.exs', [:undefined]}}
# {#PID<0.119.0>, {:fs, :file_event},
#  {'/home/louis/projects/talks/mix-test-watch/02.exs', [:modified, :closed]}}
#
#=> :ok

# Great. Let's create a GenServer to subscribe to these events.
